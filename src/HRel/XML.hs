{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.XML
    ( ParseError (..)
    , parseXml )
where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad.Except

import           Data.Attoparsec.Text hiding (skipSpace)
import           Data.Conduit
import qualified Data.Text            as Text

import Numeric (readHex)

import HRel.XML.Ranges

name :: Parser Text.Text
name =
    Text.cons
        <$> satisfy isNameStartChar
        <*> takeWhile isNameChar

skipSpace :: Parser ()
skipSpace = skip isSpace

data TextSegment
    = TextSegment Text.Text
    | CharReference Int
    | NameReference Text.Text
    deriving (Show, Eq)

reference :: Parser TextSegment
reference =
    entityRef <|> charHexRef <|> charDecRef
    where
        entityRef = do
            _ <- char '&'
            entName <- name
            _ <- char ';'
            pure (NameReference entName)

        charDecRef = do
            _ <- string "&#"
            digits <- some (satisfy isDigit)
            _ <- char ';'
            pure (CharReference (read digits))

        charHexRef = do
            _ <- string "&#x"
            digits <- some (satisfy isHexDigit)
            _ <- char ';'
            pure (CharReference (fst (head (readHex digits))))

attributeValue :: Parser [TextSegment]
attributeValue =
    quoted '"' <|> quoted '\''
    where
        quoted delim = do
            _ <- char delim
            segments <- many (inQuotes delim <|> reference)
            _ <- char delim
            pure segments

        inQuotes delim =
            TextSegment <$> takeWhile1 (\ input -> input /= '<' && input /= '&' && input /= delim)

attribute :: Parser (Text.Text, [TextSegment])
attribute = do
    attrName <- name
    skipSpace
    attrValue <- attributeValue
    skipSpace
    pure (attrName, attrValue)

startTag :: Parser (Text.Text, [(Text.Text, [TextSegment])])
startTag = do
    _ <- char '<'
    tagName <- name
    skipSpace
    tagAttrs <- many attribute
    skipSpace
    _ <- char '>'
    pure (tagName, tagAttrs)

endTag :: Parser Text.Text
endTag = do
    _ <- string "</"
    tagName <- name
    skipSpace
    _ <- char '>'
    pure tagName

emptyTag :: Parser (Text.Text, [(Text.Text, [TextSegment])])
emptyTag = do
    _ <- char '<'
    tagName <- name
    skipSpace
    tagAttrs <- many attribute
    skipSpace
    _ <- char '>'
    pure (tagName, tagAttrs)

charData :: Parser Text.Text
charData =
    Text.concat <$> many (generalBody <|> taboo)
    where
        generalBody =
            takeWhile1 $ \ input ->
                input /= '<' && input /= '&' && input /= ']'

        taboo = (string "]]>" >> empty) <|> Text.singleton <$> anyChar

comment :: Parser Text.Text
comment = do
    _ <- string "<!--"
    segments <- many (takeWhile1 (/= '-') <|> taboo)
    _ <- string "-->"
    pure (Text.concat segments)
    where
        taboo = (string "-->" >> empty) <|> Text.singleton <$> anyChar

cdataSection :: Parser Text.Text
cdataSection = do
    _ <- string "<![CDATA["
    segments <- many (takeWhile1 (/= ']') <|> taboo)
    _ <- string "]]>"
    pure (Text.concat segments)
    where
        taboo = (string "]]>" >> empty) <|> Text.singleton <$> anyChar

processInstruction :: Parser (Text.Text, Text.Text)
processInstruction = do
    _ <- string "<?"
    piName <- name
    segments <- many (takeWhile1 (/= '?') <|> taboo)
    _ <- string "?>"
    pure (piName, Text.concat segments)
    where
        taboo = (string "?>" >> empty) <|> Text.singleton <$> anyChar

data Component
    = StartTag Text.Text [(Text.Text, [TextSegment])]
    | EndTag Text.Text
    | EmptyTag Text.Text [(Text.Text, [TextSegment])]
    | Text TextSegment
    | ProcessInstruction Text.Text Text.Text
    | Comment Text.Text

component :: Parser Component
component =
    choice
        [ uncurry StartTag <$> startTag
        , EndTag <$> endTag
        , uncurry EmptyTag <$> emptyTag
        , Text . TextSegment <$> charData
        , Text . TextSegment <$> cdataSection
        , Text <$> reference
        , uncurry ProcessInstruction <$> processInstruction
        , Comment <$> comment ]

-- | Error that occurs during parsing
data ParseError =
    ParseError
        { parseErrorLeftOver :: Text.Text
        , parseErrorContexts :: [String]
        , parseErrorMessage  :: String }
    deriving (Show, Eq)

-- | Parse XML incrementally.
parseXml :: (Monad m, MonadError ParseError m) => ConduitT Text.Text Component m ()
parseXml =
    loop (Partial (parse component))
    where
        withMore handler = await >>= maybe (pure ()) handler

        loop = \case
            Fail leftOver contexts message ->
                throwError (ParseError leftOver contexts message)

            Done leftOver result -> do
                yield result
                loop (parse component leftOver)

            Partial cont ->
                withMore (loop . cont)
