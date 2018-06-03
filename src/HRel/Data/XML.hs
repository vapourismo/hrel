{-# LANGUAGE LambdaCase #-}

module HRel.Data.XML
    ( XmlTraversal
    , attribute
    , attributes
    , text
    , children
    , child )
where

import Control.Applicative

import qualified Data.ByteString as ByteString

import HRel.Data.Traversal
import HRel.Data.XML.Parser (XmlMessage (..))

-- | XML traversal
type XmlTraversal = TraversalT XmlMessage

-- | Value for an attribute.
attribute :: Functor f => ByteString.ByteString -> XmlTraversal f ByteString.ByteString
attribute needle =
    pull >>= \case
        Attribute name value
            | name == needle -> pure value
            | otherwise      -> attribute needle
        _                    -> empty

-- | Gather all attributes.
attributes :: Functor f => XmlTraversal f [(ByteString.ByteString, ByteString.ByteString)]
attributes =
    many anAttribute
    where
        anAttribute =
            pull >>= \case
                Attribute name value -> pure (name, value)
                _                    -> empty

-- | Gather all text.
text :: Functor f => XmlTraversal f ByteString.ByteString
text =
    ByteString.concat <$> many step
    where
        step =
            pull >>= \case
                Text text -> pure text
                _         -> step

-- | Skip everything until exiting the current node.
skipNode :: Functor f => ByteString.ByteString -> XmlTraversal f a -> XmlTraversal f a
skipNode name cont =
    pull >>= \case
        Open openName                       -> skipNode openName (skipNode name cont)
        Close closeName | closeName == name -> cont
        _                                   -> skipNode name cont

-- | Traverse all child nodes with the given name.
children :: Functor f => ByteString.ByteString -> XmlTraversal f a -> XmlTraversal f [a]
children name action = many (child name action)

-- | Traverse a child node with the given name.
child :: Functor f => ByteString.ByteString -> XmlTraversal f a -> XmlTraversal f a
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
