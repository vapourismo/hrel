{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.XML
    ( TextSegment (..)
    , Component (..)
    , ParseError (..)
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

-- | Name
name :: Parser Text.Text
name =
    Text.cons
        <$> satisfy isNameStartChar
        <*> takeWhile isNameChar

-- | Skip over space.
skipSpace :: Parser ()
skipSpace = skip isSpace

-- | Text segment
data TextSegment
    = TextSegment Text.Text   -- ^ Normal text
    | CharReference Int       -- ^ Character reference (e.g. @&#1234;@ or @&#xABCD;@)
    | NameReference Text.Text -- ^ Name reference (e.g. @&amp;@ or @&quot;@)
    deriving (Show, Eq)

-- | 'CharReference' or 'NameReference'
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

-- | Attribute value in single or double quotes
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

-- | Attribute
attribute :: Parser (Text.Text, [TextSegment])
attribute = do
    attrName <- name
    skipSpace
    attrValue <- attributeValue
    skipSpace
    pure (attrName, attrValue)

-- | Opening tag
openingTag :: Parser (Text.Text, [(Text.Text, [TextSegment])])
openingTag = do
    _ <- char '<'
    tagName <- name
    skipSpace
    tagAttrs <- many attribute
    skipSpace
    _ <- char '>'
    pure (tagName, tagAttrs)

-- | Closing tag
closingTag :: Parser Text.Text
closingTag = do
    _ <- string "</"
    tagName <- name
    skipSpace
    _ <- char '>'
    pure tagName

-- | Empty tag
emptyTag :: Parser (Text.Text, [(Text.Text, [TextSegment])])
emptyTag = do
    _ <- char '<'
    tagName <- name
    skipSpace
    tagAttrs <- many attribute
    skipSpace
    _ <- char '>'
    pure (tagName, tagAttrs)

-- | Character data
charData :: Parser Text.Text
charData =
    Text.concat <$> many (generalBody <|> taboo)
    where
        generalBody =
            takeWhile1 $ \ input ->
                input /= '<' && input /= '&' && input /= ']'

        taboo = (string "]]>" >> empty) <|> Text.singleton <$> anyChar

-- | Comment
comment :: Parser Text.Text
comment = do
    _ <- string "<!--"
    segments <- many (takeWhile1 (/= '-') <|> taboo)
    _ <- string "-->"
    pure (Text.concat segments)
    where
        taboo = (string "-->" >> empty) <|> Text.singleton <$> anyChar

-- | CData section
cdataSection :: Parser Text.Text
cdataSection = do
    _ <- string "<![CDATA["
    segments <- many (takeWhile1 (/= ']') <|> taboo)
    _ <- string "]]>"
    pure (Text.concat segments)
    where
        taboo = (string "]]>" >> empty) <|> Text.singleton <$> anyChar

-- | Process instructions
processInstruction :: Parser (Text.Text, Text.Text)
processInstruction = do
    _ <- string "<?"
    piName <- name
    segments <- many (takeWhile1 (/= '?') <|> taboo)
    _ <- string "?>"
    pure (piName, Text.concat segments)
    where
        taboo = (string "?>" >> empty) <|> Text.singleton <$> anyChar

-- | XML componetn
data Component
    = StartTag Text.Text [(Text.Text, [TextSegment])] -- ^ @<tag ...>@
    | EndTag Text.Text                                -- ^ @</tag>@
    | EmptyTag Text.Text [(Text.Text, [TextSegment])] -- ^ @<tag ... />@
    | Text TextSegment                                -- ^ Text segments
    | ProcessInstruction Text.Text Text.Text          -- ^ @<?name ... ?>@
    | Comment Text.Text                               -- ^ @<!-- ... -->@

-- | XML component
component :: Parser Component
component =
    choice
        [ uncurry StartTag <$> openingTag
        , EndTag <$> closingTag
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
