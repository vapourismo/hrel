{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.XML
    ( XmlParser
    , runXmlTraversal
    , attribute
    , attributes
    , text
    , children
    , child )
where

import Control.Applicative

import qualified Data.ByteString as ByteString

import qualified Xeno.SAX   as XML
import qualified Xeno.Types as XML

import HRel.Data.Parser

-- | Fold message
data Message
    = Open ByteString.ByteString
    | Attribute ByteString.ByteString ByteString.ByteString
    | OpenEnd ByteString.ByteString
    | Close ByteString.ByteString
    | Text ByteString.ByteString
    | Terminate
    deriving (Show, Eq)

-- | XML parser
type XmlParser = Parser Message

-- | Run the 'XmlParser' against an XML document.
runXmlTraversal :: XmlParser a -> ByteString.ByteString -> Either XML.XenoException a
runXmlTraversal baseAction contents = do
    result <-
        XML.fold
            (\ action name -> feed (Open name) action)
            (\ action name value -> feed (Attribute name value) action)
            (\ action name -> feed (OpenEnd name) action)
            (\ action value -> feed (Text value) action)
            (\ action name -> feed (Close name) action)
            (\ action value -> feed (Text value) action)
            baseAction
            contents

    case terminate result of
        Nothing -> Left (XML.XenoParseError "Fail")
        Just x  -> Right x

-- | Value for an attribute.
attribute :: ByteString.ByteString -> XmlParser ByteString.ByteString
attribute needle =
    pull >>= \case
        Attribute name value
            | name == needle -> pure value
            | otherwise      -> attribute needle
        _                    -> empty

-- | Gather all attributes.
attributes :: XmlParser [(ByteString.ByteString, ByteString.ByteString)]
attributes =
    step []
    where
        step state =
            pull >>= \case
                Attribute name value -> step ((name, value) : state)
                _                    -> pure state

-- | Gather all text.
text :: XmlParser ByteString.ByteString
text =
    ByteString.concat <$> many step
    where
        step =
            pull >>= \case
                Text text -> pure text
                _         -> step

-- | Skip everything until exiting the current node.
skipNode :: ByteString.ByteString -> XmlParser a -> XmlParser a
skipNode name cont =
    pull >>= \case
        Open openName                       -> skipNode openName (skipNode name cont)
        Close closeName | closeName == name -> cont
        _                                   -> skipNode name cont

-- | Traverse all child nodes with the given name.
children :: ByteString.ByteString -> XmlParser a -> XmlParser [a]
children name action = many (child name action)

-- | Traverse a child node with the given name.
child :: ByteString.ByteString -> XmlParser a -> XmlParser a
child tagName baseAction =
    withRoot
    where
        withRoot =
            pull >>= \case
                Open name
                    | tagName == name -> withInside baseAction (0 :: Word)
                    | otherwise       -> skipNode name withRoot
                _                     -> withRoot

        withInside action depth =
            await >>= \case
                Nothing      -> seal action
                Just message -> handleInside action depth message

        handleInside action depth = \case
            message@(Open name) | tagName == name ->
                withInside (feed message action) (depth + 1)

            message@(Close name) | tagName == name ->
                if depth > 0 then
                    withInside (feed message action) (depth - 1)
                else
                    seal action

            message -> withInside (feed message action) depth
