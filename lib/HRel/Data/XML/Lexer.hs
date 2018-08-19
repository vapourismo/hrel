{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Data.XML.Lexer
    ( Token (..)
    , tokenToByteString
    , token
    )
where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad

import           Data.Attoparsec.ByteString
import qualified Data.ByteString            as ByteString
import           Data.Word

quote :: Word8 -> Parser ByteString.ByteString
quote delim = do
    skip (== delim)
    segments <- many (quoteBody <|> escapeChar)
    skip (== delim)
    pure (ByteString.concat segments)
    where
        quoteBody = takeWhile1 (\ input -> input /= delim && input /= 92)
        escapeChar = (\ a b -> ByteString.pack [a, b]) <$> word8 92 <*> anyWord8

instructionOpen :: Parser ()
instructionOpen = void (string "<?")

instructionClose :: Parser ()
instructionClose = void (string "?>")

docTypeOpen :: Parser ()
docTypeOpen = void (string "<!DOCTYPE")

commentOpen :: Parser ()
commentOpen = void (string "<!--")

commentClose :: Parser ()
commentClose = void (string "-->")

cdataOpen :: Parser ()
cdataOpen = void (string "<![CDATA[")

cdataClose :: Parser ()
cdataClose = void (string "]]>")

openTagBegin :: Parser ()
openTagBegin = void (string "<")

closeTagBegin :: Parser ()
closeTagBegin = void (string "</")

openTagEnd :: Parser ()
openTagEnd = void (string ">")

openTagCloseEnd :: Parser ()
openTagCloseEnd = void (string "/>")

whitespace :: Parser ByteString.ByteString
whitespace = takeWhile1 (`elem` [0x20, 0x9, 0xD, 0xA])

equalsSign :: Parser ()
equalsSign = void (word8 61)

name :: Parser ByteString.ByteString
name = do
    header <- satisfy isNameStartChar
    ByteString.cons header <$> takeWhile isNameChar
    where
        isNameStartChar input =
            input == 58
            || 65 <= input && input <= 90
            || input == 95
            || 97 <= input && input <= 122

        isNameChar input =
            isNameStartChar input
            || input == 45
            || input == 46
            || 48 <= input && input <= 57

-- | Xml token
data Token
    = Quote Word8 ByteString.ByteString
    | DocTypeOpen
    | InstructionOpen
    | InstructionClose
    | CommentOpen
    | CommentClose
    | CDataOpen
    | CDataClose
    | CloseTagBegin
    | OpenTagBegin
    | OpenTagEnd
    | OpenTagCloseEnd
    | Whitespace ByteString.ByteString
    | EqualsSign
    | Name ByteString.ByteString
    | Other Word8
    deriving (Show, Eq)

-- | Convert an XML 'Token' to 'ByteString.ByteString'.
tokenToByteString :: Token -> ByteString.ByteString
tokenToByteString = \case
    Quote delim inner -> ByteString.cons delim (ByteString.snoc inner delim)
    DocTypeOpen       -> "<!DOCTYPE"
    InstructionOpen   -> "<?"
    InstructionClose  -> "?>"
    CommentOpen       -> "<!--"
    CommentClose      -> "-->"
    CDataOpen         -> "<![CDATA["
    CDataClose        -> "]]>"
    CloseTagBegin     -> "</"
    OpenTagBegin      -> "<"
    OpenTagEnd        -> ">"
    OpenTagCloseEnd   -> "/>"
    Whitespace inner  -> inner
    EqualsSign        -> "="
    Name name         -> name
    Other char        -> ByteString.singleton char

-- | Parser for a 'Token'.
token :: Parser Token
token =
    choice
        [ Quote 34 <$> quote 34
        , Quote 39 <$> quote 39
        , DocTypeOpen <$ docTypeOpen
        , InstructionOpen <$ instructionOpen
        , InstructionClose <$ instructionClose
        , CommentOpen <$ commentOpen
        , CommentClose <$ commentClose
        , CDataOpen <$ cdataOpen
        , CDataClose <$ cdataClose
        , CloseTagBegin <$ closeTagBegin
        , OpenTagBegin <$ openTagBegin
        , OpenTagEnd <$ openTagEnd
        , OpenTagCloseEnd <$ openTagCloseEnd
        , EqualsSign <$ equalsSign
        , Whitespace <$> whitespace
        , Name <$> name
        , Other <$> anyWord8
        ]
