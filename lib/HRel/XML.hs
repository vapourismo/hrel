{-# LANGUAGE OverloadedStrings #-}

module HRel.XML (
	xml,
	content,
	Content (..),
	Attribute
) where

import           Control.Monad
import           Control.Applicative

import           Data.Char
import           Data.Bits
import           Data.List
import qualified Data.Text as T

import           Data.Attoparsec.Text as A hiding (space)

isXMLChar :: Char -> Bool
isXMLChar c =
	c == '\t'
	|| c == '\n'
	|| c == '\r'
	|| (0x20 <= n && n <= 0xD7FF)
	|| (0xE000 <= n && n <= 0xFFFD)
	|| (0x10000 <= n && n <= 0x10FFFF)
	where n = ord c

space :: Parser ()
space =
	skipWhile (\ c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

charEntity :: Parser T.Text
charEntity = do
	char '&'
	msum [string "#x" >> hexChar, string "#" >> decChar, entName] <* char ';'
	where
		fromChar x =
			case x of
				'1' -> 1; '2' -> 2; '3' -> 3; '4' -> 4; '5' -> 5; '6' -> 6; '7' -> 7; '8' -> 8;
				'9' -> 9; 'A' -> 10; 'B' -> 11; 'C' -> 12; 'D' -> 13; 'E' -> 14; 'F' -> 16; _ -> 0

		foldChars =
			foldl' (\ n c -> shiftL n 4 .|. fromChar c) 0

		hexChar =
			T.singleton . chr . foldChars . T.unpack <$> A.takeWhile1 isHexDigit

		decChar =
			T.singleton . chr . foldChars . T.unpack <$> A.takeWhile1 isDigit

		fromName n =
			case n of
				"lt"   -> "<"
				"gt"   -> ">"
				"amp"  -> "&"
				"quot" -> "\""
				"apos" -> "'"
				_      -> T.cons '&' (T.snoc n ';')

		entName =
			fromName <$> name

clearEntities :: T.Text -> T.Text
clearEntities input =
	extractResult' (parse parser input)
	where
		parser = many (A.takeWhile1 (/= '&') <|> charEntity <|> (T.singleton <$> char '&'))

		extractResult' (Partial f) = extractResult (f T.empty)
		extractResult' x           = extractResult x

		extractResult (Done rest body) = T.concat (body ++ [rest])
		extractResult _                = input

name :: Parser T.Text
name =
	T.cons <$> satisfy startChar <*> A.takeWhile bodyChar
	where
		startChar ':' = True
		startChar '_' = True
		startChar c = let n = ord c in
			isAlpha c
			|| (0xC0 <= n && n <= 0xF6 && n /= 0xD7)
			|| (0xF8 <= n && n <= 0x2FF)
			|| (0x370 <= n && n <= 0x1FFF && n /= 0x37E)
			|| (0x200C <= n && n <= 0x200D)
			|| (0x2070 <= n && n <= 0x218F)
			|| (0x2C00 <= n && n <= 0x2FEF)
			|| (0x3001 <= n && n <= 0xD7FF)
			|| (0xF900 <= n && n <= 0xFDCF)
			|| (0xFDF0 <= n && n <= 0xFFFD)
			|| (0x10000 <= n && n <= 0xEFFF)

		bodyChar '-' = True
		bodyChar '.' = True
		bodyChar c = let n = ord c in
			startChar c
			|| isDigit c
			|| n == 0xB7
			|| (0x0300 <= n && n <= 0x036F)
			|| (0x203F <= n && n <= 0x2040)

charData :: Parser T.Text
charData =
	clearEntities <$> A.takeWhile1 (/= '<')

comment :: Parser T.Text
comment = do
	string "!--"
	T.pack <$> manyTill (satisfy isXMLChar) (string "-->") <* string "-->"

cData :: Parser T.Text
cData = do
	string "![CDATA["
	T.pack <$> manyTill (satisfy isXMLChar) (string "-->") <* string "]]>"

openTag :: Parser (T.Text, [Attribute])
openTag = do
	(,) <$> name
	    <*> many (space >> attribute)
	    <*  space
	    <*  char '>'

emptyTag :: Parser (T.Text, [Attribute])
emptyTag = do
	(,) <$> name
	    <*> many (space >> attribute)
	    <*  space
	    <*  string "/>"

closeTag :: Parser T.Text
closeTag = do
	string "/"
	name <* space <* char '>'

attributeValue :: Parser T.Text
attributeValue = do
	(char '\'' *> A.takeWhile (/= '\'') <* char '\'')
	<|> (char '"' *> A.takeWhile (/= '"') <* char '"')

type Attribute = (T.Text, T.Text)

attribute :: Parser Attribute
attribute =
	(,) <$> name
	    <*  space
	    <*  char '='
	    <*  space
	    <*> (clearEntities <$> attributeValue)

instruction :: Parser (T.Text, T.Text)
instruction = do
	string "?"
	(,) <$> name
	    <*  space
	    <*> (T.strip . T.pack <$> manyTill (satisfy isXMLChar) (string "?>"))
	    <*  string "?>"

systemLiteral :: Parser ()
systemLiteral =
	(char '\'' *> skipWhile (/= '\'') <* char '\'')
	<|> (char '"' *> skipWhile (/= '"') <* char '"')

publicIDLiteral :: Parser ()
publicIDLiteral =
	(char '\'' *> skipWhile (\ c -> c /= '\'' && isPublicIDChar c) <* char '\'')
	<|> (char '"' *> skipWhile (isPublicIDChar) <* char '"')
	where
		isPublicIDChar c =
			c == ' '
			|| c == '\r'
			|| c == '\n'
			|| ('a' <= c && c <= 'z')
			|| ('A' <= c && c <= 'Z')
			|| ('0' <= c && c <= '9')
			|| elem c ("-'()+,./:=?;!*#@$_%" :: String)

externalID :: Parser ()
externalID = do
	(string "SYSTEM" >> space >> systemLiteral)
	<|> (string "PUBLIC" >> space >> publicLiteral)
	where
		publicLiteral = do
			publicIDLiteral
			space
			systemLiteral

declSep :: Parser ()
declSep = space <|> (char '%' *> void name <* char ';')

sepBy2 :: Parser a -> Parser s -> Parser [a]
sepBy2 a sep =
	(:) <$> a
	    <*  sep
	    <*> sepBy1 a sep

elementDecl :: Parser ()
elementDecl = do
	string "<!ELEMENT"
	space
	name
	contentSpec
	space
	() <$ char '>'
	where
		contentSpec =
			msum [() <$ string "EMPTY",
			      () <$ string "ANY",
			      mixedOne <|> mixedTwo,
			      children]

		mixedOne = do
			char '('
			space
			string "#PCDATA"
			many (space >> char '|' >> space >> name)
			space
			() <$ string ")*"

		mixedTwo = do
			char '('
			space
			string "#PCDATA"
			space
			() <$ char ')'

		children = do
			choice <|> seq
			() <$ optional (satisfy (`elem` ("?*+" :: String)))

		choice = do
			char '('
			sepBy2 (space >> cp) (space >> char '|')
			space
			() <$ char ')'

		seq = do
			char '('
			sepBy1 (space >> cp) (space >> char ',')
			space
			() <$ char ')'

		cp = do
			void name <|> choice <|> seq
			() <$ optional (satisfy (`elem` ("?*+" :: String)))

-- attributeListDecl :: Parser ()
-- attributeListDecl = do
-- 	string "<!ATTLIST"
-- 	space
-- 	name
-- 	many attDef
-- 	space
-- 	() <$ char '>'
-- 	where
-- 		attDef = do
-- 			space
-- 			name
-- 			space
-- 			attType
-- 			space
-- 			defaultDecl

-- 		attType = undefined

-- 		defaultDecl = undefined

-- entityDecl :: Parser ()
-- entityDecl = undefined

-- notationDecl :: Parser ()
-- notationDecl = undefined

markupDecl :: Parser ()
markupDecl =
	msum [elementDecl,
	      -- attributeListDecl,
	      -- entityDecl,
	      -- notationDecl,
	      void instruction,
	      void comment]

intSubSet :: Parser ()
intSubSet =
	() <$ many (markupDecl <|> declSep)

docType :: Parser ()
docType = do
	string "!DOCTYPE"
	space
	name
	optional (space >> externalID)
	space
	optional (char '[' *> intSubSet <* char ']')
	space
	() <$ char '>'

data Content
	= Text T.Text
	| Comment T.Text
	| Open T.Text [Attribute]
	| Close T.Text
	| Empty T.Text [Attribute]
	| Instruction T.Text T.Text
	| DocType
	deriving (Show, Eq, Ord)

content :: Parser Content
content =
	angleOpened <|> (Text <$> charData)
	where
		angleOpened = do
			char '<'
			msum [uncurry Open <$> openTag,
			      Close <$> closeTag,
			      uncurry Empty <$> emptyTag,
			      uncurry Instruction <$> instruction,
			      Text <$> cData,
			      DocType <$ docType,
			      Comment <$> comment]

xml :: Parser [Content]
xml = many content <* endOfInput
