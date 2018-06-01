{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.XML
    ( XmlTraversal
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

-- | Fold message
data Message
    = Open ByteString.ByteString
    | Attribute ByteString.ByteString ByteString.ByteString
    | OpenEnd ByteString.ByteString
    | Close ByteString.ByteString
    | Text ByteString.ByteString
    | Terminate
    deriving (Show, Eq)

-- | XML traversal
data XmlTraversal a
    = Fail
    | Pure a
    | With (Message -> XmlTraversal a)

instance Functor XmlTraversal where
    fmap f = \case
        Fail         -> Fail
        Pure x       -> Pure (f x)
        With handler -> With (fmap f . handler)

instance Applicative XmlTraversal where
    pure = Pure

    Fail     <*> _        = Fail
    _        <*> Fail     = Fail
    Pure f   <*> rhs      = f <$> rhs
    lhs      <*> Pure x   = ($ x) <$> lhs
    With lhs <*> With rhs = With (\ message -> lhs message <*> rhs message)

instance Alternative XmlTraversal where
    empty = Fail

    Fail         <|> rhs      = rhs
    lhs          <|> Fail     = lhs
    Pure x       <|> _        = Pure x
    With lhs     <|> With rhs = With (\ message -> lhs message <|> rhs message)
    With handler <|> rhs      = With (\ message -> handler message <|> rhs)

-- | Delegate a 'Message' to another 'XmlTraversal'.
delegate :: Message -> XmlTraversal a -> XmlTraversal a
delegate message (With handler) = handler message
delegate _       other          = other

-- | Run the 'XmlTraversal' against an XML document.
runXmlTraversal :: XmlTraversal a -> ByteString.ByteString -> Either XML.XenoException a
runXmlTraversal baseAction contents = do
    result <-
        XML.fold
            (\ action name -> delegate (Open name) action)
            (\ action name value -> delegate (Attribute name value) action)
            (\ action name -> delegate (OpenEnd name) action)
            (\ action value -> delegate (Text value) action)
            (\ action name -> delegate (Close name) action)
            (\ action value -> delegate (Text value) action)
            baseAction
            contents

    case delegate Terminate result of
        Fail   -> Left (XML.XenoParseError "Fail")
        Pure x -> Right x
        With _ -> Left (XML.XenoParseError "Incomplete")

-- | Value for an attribute.
attribute :: ByteString.ByteString -> XmlTraversal ByteString.ByteString
attribute needle =
    With $ \case
        Attribute name value
            | name == needle -> Pure value
            | otherwise      -> attribute needle
        _                    -> Fail

-- | Gather all attributes.
attributes :: XmlTraversal [(ByteString.ByteString, ByteString.ByteString)]
attributes =
    step []
    where
        step state =
            With $ \case
                Attribute name value -> step ((name, value) : state)
                _                    -> Pure state

-- | Gather all text.
text :: XmlTraversal ByteString.ByteString
text =
    ByteString.concat <$> step
    where
        step =
            With $ \case
                Terminate -> Pure []
                Text text -> (text :) <$> step
                _         -> step

-- | Traverse all child nodes with the given name.
children :: ByteString.ByteString -> XmlTraversal a -> XmlTraversal [a]
children tagName baseAction =
    withRoot
    where
        withRoot =
            With $ \case
                Open name
                    | tagName == name -> withInside baseAction (0 :: Word)
                    | otherwise       -> skipNode name withRoot
                Terminate             -> Pure []
                _                     -> withRoot

        skipNode name cont =
            With $ \case
                Open openName                       -> skipNode openName (skipNode name cont)
                Close closeName | closeName == name -> cont
                _                                   -> skipNode name cont

        withInside action depth =
            With $ \case
                Terminate ->
                    case delegate Terminate action of
                        Pure x -> Pure [x]
                        _      -> Pure []

                message@(Open name) | tagName == name ->
                    withInside (delegate message action) (depth + 1)

                message@(Close name) | tagName == name ->
                    if depth > 0 then
                        withInside (delegate message action) (depth - 1)
                    else
                        case delegate Terminate action of
                            Pure x -> (x :) <$> withRoot
                            _      -> withRoot

                message -> withInside (delegate message action) depth

-- | Traverse a child node with the given name.
child :: ByteString.ByteString -> XmlTraversal a -> XmlTraversal a
child tagName baseAction =
    withRoot
    where
        withRoot =
            With $ \case
                Open name
                    | tagName == name -> withInside baseAction (0 :: Word)
                    | otherwise       -> skipNode name withRoot
                Terminate             -> Fail
                _                     -> withRoot

        skipNode name cont =
            With $ \case
                Open openName                       -> skipNode openName (skipNode name cont)
                Close closeName | closeName == name -> cont
                _                                   -> skipNode name cont

        withInside action depth =
            With $ \case
                Terminate -> delegate Terminate action

                message@(Open name) | tagName == name ->
                    withInside (delegate message action) (depth + 1)

                message@(Close name) | tagName == name ->
                    if depth > 0 then
                        withInside (delegate message action) (depth - 1)
                    else
                        delegate Terminate action

                message -> withInside (delegate message action) depth
