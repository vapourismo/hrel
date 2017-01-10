{-# LANGUAGE OverloadedStrings #-}

module HRel.Parser2 (
	xml,
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

exact :: Char -> Parser Char
exact c = satisfy (== c)

stringUntil :: T.Text -> (Char -> Bool) -> T.Text -> Maybe (T.Text, T.Text)
stringUntil term cond =
	action
	where
		action input =
			case T.break breaker input of
				(body, rest) | T.isPrefixOf (T.take 1 term) rest && not (T.isPrefixOf term rest) ->
					case action (T.tail rest) of
						Just (body', rest') ->
							Just (T.concat [body, T.take 1 term, body'], rest')
						Nothing ->
							Just (T.snoc body (T.head term), T.tail rest)

				("", _) -> Nothing

				x -> Just x

		breaker c = c == T.head term || not (cond c)

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
	exact '&'
	msum [string "#x" >> hexChar, string "#" >> decChar, entName] <* exact ';'
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
		parser = many (A.takeWhile1 (/= '&') <|> charEntity <|> (T.singleton <$> exact '&'))

		extractResult' (Partial f) = extractResult (f T.empty)
		extractResult' x           = extractResult x

		extractResult (Done rest body) = T.concat (body ++ [rest])
		extractResult _                = input

name :: Parser T.Text
name =
	T.cons <$> satisfy startChar <*> A.takeWhile bodyChar
	where
		startChar c = let n = ord c in
			c == ':'
			|| c == '_'
			|| ('A' <= c && c <= 'Z')
			|| ('a' <= c && c <= 'z')
			|| (0xC0 <= n && n <= 0xD6)
			|| (0xD8 <= n && n <= 0xF6)
			|| (0xF8 <= n && n <= 0x2FF)
			|| (0x370 <= n && n <= 0x37D)
			|| (0x37F <= n && n <= 0x1FFF)
			|| (0x200C <= n && n <= 0x200D)
			|| (0x2070 <= n && n <= 0x218F)
			|| (0x2C00 <= n && n <= 0x2FEF)
			|| (0x3001 <= n && n <= 0xD7FF)
			|| (0xF900 <= n && n <= 0xFDCF)
			|| (0xFDF0 <= n && n <= 0xFFFD)
			|| (0x10000 <= n && n <= 0xEFFF)

		bodyChar c = let n = ord c in
			startChar c
			|| c == '-'
			|| c == '.'
			|| n == 0xB7
			|| ('0' <= c && c <= '9')
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
	    <*  exact '>'

emptyTag :: Parser (T.Text, [Attribute])
emptyTag = do
	(,) <$> name
	    <*> many (space >> attribute)
	    <*  space
	    <*  string "/>"

closeTag :: Parser T.Text
closeTag = do
	string "/"
	name <* space <* exact '>'

attributeValue :: Parser T.Text
attributeValue = do
	(exact '\'' *> A.takeWhile (/= '\'') <* exact '\'')
	<|> (exact '"' *> A.takeWhile (/= '"') <* exact '"')

type Attribute = (T.Text, T.Text)

attribute :: Parser Attribute
attribute =
	(,) <$> name
	    <*  space
	    <*  exact '='
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
	(exact '\'' *> skipWhile (/= '\'') <* exact '\'')
	<|> (exact '"' *> skipWhile (/= '"') <* exact '"')

publicIDLiteral :: Parser ()
publicIDLiteral =
	(exact '\'' *> skipWhile (\ c -> c /= '\'' && isPublicIDChar c) <* exact '\'')
	<|> (exact '"' *> skipWhile (isPublicIDChar) <* exact '"')
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
declSep = space <|> (exact '%' *> void name <* exact ';')

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
	() <$ exact '>'
	where
		contentSpec =
			msum [() <$ string "EMPTY",
			      () <$ string "ANY",
			      mixedOne <|> mixedTwo,
			      children]

		mixedOne = do
			exact '('
			space
			string "#PCDATA"
			many (space >> exact '|' >> space >> name)
			space
			() <$ string ")*"

		mixedTwo = do
			exact '('
			space
			string "#PCDATA"
			space
			() <$ exact ')'

		children = do
			choice <|> seq
			() <$ optional (satisfy (`elem` ("?*+" :: String)))

		choice = do
			exact '('
			sepBy2 (space >> cp) (space >> exact '|')
			space
			() <$ exact ')'

		seq = do
			exact '('
			sepBy1 (space >> cp) (space >> exact ',')
			space
			() <$ exact ')'

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
-- 	() <$ exact '>'
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
	optional (exact '[' *> intSubSet <* exact ']')
	space
	() <$ exact '>'

data Content
	= Text T.Text
	| Comment T.Text
	| Open T.Text [Attribute]
	| Close T.Text
	| Empty T.Text [Attribute]
	| Instruction T.Text T.Text
	| DocType
	deriving (Show, Eq, Ord)

contents :: Parser [Content]
contents =
	many (angleOpened <|> (Text <$> charData))
	where
		angleOpened = do
			exact '<'
			msum [uncurry Open <$> openTag,
			      Close <$> closeTag,
			      uncurry Empty <$> emptyTag,
			      uncurry Instruction <$> instruction,
			      Text <$> cData,
			      DocType <$ docType,
			      Comment <$> comment]

xml :: Parser [Content]
xml = contents <* endOfInput
