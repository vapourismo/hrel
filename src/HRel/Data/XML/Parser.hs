{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module HRel.Data.XML.Parser
    ( XmlMessage (..)
    , XmlError (..)
    , subscribeToXml )
where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.State

import           Data.Attoparsec.ByteString
import qualified Data.ByteString            as ByteString
import           Data.Conduit

import HRel.Data.XML.Lexer

data XmlMessage
    = Open ByteString.ByteString
    | Attribute ByteString.ByteString ByteString.ByteString
    | OpenEnd ByteString.ByteString
    | Close ByteString.ByteString
    | Text ByteString.ByteString
    deriving (Show, Eq)

type XmlParser = StateT (Maybe ByteString.ByteString) Parser

afterSpaces :: XmlParser Token
afterSpaces =
    lift token >>= \case
        Whitespace _ -> afterSpaces
        message      -> pure message

open :: XmlParser XmlMessage
open = do
    Nothing <- get

    OpenTagBegin <- lift token
    Name name    <- lift token

    put (Just name)
    pure (Open name)

attribute :: XmlParser XmlMessage
attribute = do
    Just _ <- get

    Name name   <- afterSpaces
    EqualsSign  <- afterSpaces
    Quote _ value <- afterSpaces

    pure (Attribute name value)

openEnd :: XmlParser [XmlMessage]
openEnd =
    normalEnd <|> immediateClose
    where
        normalEnd = do
            Just name <- get
            OpenTagEnd <- afterSpaces
            put Nothing
            pure [OpenEnd name]

        immediateClose = do
            Just name <- get
            OpenTagCloseEnd <- afterSpaces
            put Nothing
            pure [OpenEnd name, Close name]

close :: XmlParser XmlMessage
close = do
    CloseTagBegin <- lift token
    Name name     <- lift token
    OpenTagEnd    <- lift token
    pure (Close name)

text :: XmlParser XmlMessage
text = do
    Nothing <- get
    Text . ByteString.concat <$> some inner
    where
        inner =
            lift token >>= \case
                tok@Quote{}      -> pure (tokenToByteString tok)
                tok@Whitespace{} -> pure (tokenToByteString tok)
                tok@EqualsSign{} -> pure (tokenToByteString tok)
                tok@Name{}       -> pure (tokenToByteString tok)
                tok@Other{}      -> pure (tokenToByteString tok)
                _                -> empty

takeUntil :: Token -> XmlParser [Token]
takeUntil delim = do
    token <- lift token
    if token == delim then
        pure []
    else
        (:) token <$> takeUntil delim

skipUntil :: Token -> XmlParser ()
skipUntil delim = do
    token <- lift token
    if token == delim then
        pure ()
    else
        skipUntil delim

cdata :: XmlParser XmlMessage
cdata = do
    Nothing <- get

    CDataOpen  <- lift token
    Text . ByteString.concat . map tokenToByteString <$> takeUntil CDataClose

instruction :: XmlParser ()
instruction = do
    Nothing <- get

    InstructionOpen <- lift token
    skipUntil InstructionClose

comment :: XmlParser ()
comment = do
    Nothing <- get

    CommentOpen <- lift token
    skipUntil CommentClose

doctype :: XmlParser ()
doctype = do
    Nothing <- get

    DocTypeOpen <- lift token
    skipUntil OpenTagEnd

messages :: XmlParser [XmlMessage]
messages =
    choice
        [ (: []) <$> open
        , (: []) <$> attribute
        , openEnd
        , (: []) <$> close
        , (: []) <$> text
        , (: []) <$> cdata
        , [] <$ instruction
        , [] <$ comment
        , [] <$ doctype ]

data XmlError =
    XmlError [String] String
    deriving (Show, Eq)

subscribeToXml :: MonadError XmlError m => ConduitT ByteString.ByteString XmlMessage m ()
subscribeToXml = do
    step (parseMessages Nothing ByteString.empty)
    where
        parseMessages =
            parse . runStateT messages

        step = \case
            Done leftOver (messages, state) -> do
                traverse yield messages
                step (parseMessages state leftOver)

            Partial cont -> do
                input <- await
                maybe (pure ()) (step . cont) input

            Fail _ contexts message ->
                throwError (XmlError contexts message)
