{-# LANGUAGE OverloadedStrings #-}

module HRel.Parser.XML (
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

import           HRel.Parser

type P = Parser T.Text

single :: P Char
single = inspect T.uncons

satisfy :: (Char -> Bool) -> P Char
satisfy = validate single

exact :: Char -> P Char
exact c = satisfy (== c)

string :: T.Text -> P T.Text
string txt =
	inspect $ \ src ->
		if T.isPrefixOf txt src then
			Just (txt, T.drop (T.length txt) src)
		else
			Nothing

satisfySome :: (Char -> Bool) -> P T.Text
satisfySome cond =
	inspect $ \ src ->
		case T.span cond src of
			(x, _) | T.null x -> Nothing
			x                 -> Just x

satisfyMany :: (Char -> Bool) -> P T.Text
satisfyMany cond =
	inspect (Just . T.span cond)

discardMany :: (Char -> Bool) -> P ()
discardMany cond =
	inspect (\ src -> Just ((), T.dropWhile cond src))

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

space :: P ()
space =
	discardMany (\ c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

charEntity :: P T.Text
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
			T.singleton . chr . foldChars . T.unpack <$> satisfySome isHexDigit

		decChar =
			T.singleton . chr . foldChars . T.unpack <$> satisfySome isDigit

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
	extractResult (runParser parser input)
	where
		parser = many (satisfySome (/= '&') <|> charEntity <|> (T.singleton <$> exact '&'))

		extractResult (Just body, rest) = T.concat (body ++ [rest])
		extractResult _                 = input

name :: P T.Text
name =
	T.cons <$> satisfy startChar <*> satisfyMany bodyChar
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

charData :: P T.Text
charData =
	clearEntities <$> satisfySome (/= '<')

comment :: P T.Text
comment = do
	string "<!--"
	inspect (stringUntil "-->" isXMLChar) <* string "-->"

cData :: P T.Text
cData = do
	string "<![CDATA["
	inspect (stringUntil "]]>" isXMLChar) <* string "]]>"

openTag :: P (T.Text, [Attribute])
openTag = do
	exact '<'
	(,) <$> name
	    <*> many (space >> attribute)
	    <*  space
	    <*  exact '>'

emptyTag :: P (T.Text, [Attribute])
emptyTag = do
	exact '<'
	(,) <$> name
	    <*> many (space >> attribute)
	    <*  space
	    <*  string "/>"

closeTag :: P T.Text
closeTag = do
	string "</"
	name <* space <* exact '>'

attributeValue :: P T.Text
attributeValue = do
	(exact '\'' *> satisfyMany (/= '\'') <* exact '\'')
	<|> (exact '"' *> satisfyMany (/= '"') <* exact '"')

type Attribute = (T.Text, T.Text)

attribute :: P Attribute
attribute =
	(,) <$> name
	    <*  space
	    <*  exact '='
	    <*  space
	    <*> (clearEntities <$> attributeValue)

instruction :: P (T.Text, T.Text)
instruction = do
	string "<?"
	(,) <$> name
	    <*  space
	    <*> (T.strip <$> inspect (stringUntil "?>" isXMLChar))
	    <*  string "?>"

systemLiteral :: P ()
systemLiteral =
	(exact '\'' *> discardMany (/= '\'') <* exact '\'')
	<|> (exact '"' *> discardMany (/= '"') <* exact '"')

publicIDLiteral :: P ()
publicIDLiteral =
	(exact '\'' *> discardMany (\ c -> c /= '\'' && isPublicIDChar c) <* exact '\'')
	<|> (exact '"' *> discardMany (isPublicIDChar) <* exact '"')
	where
		isPublicIDChar c =
			c == ' '
			|| c == '\r'
			|| c == '\n'
			|| ('a' <= c && c <= 'z')
			|| ('A' <= c && c <= 'Z')
			|| ('0' <= c && c <= '9')
			|| elem c ("-'()+,./:=?;!*#@$_%" :: String)

externalID :: P ()
externalID = do
	(string "SYSTEM" >> space >> systemLiteral)
	<|> (string "PUBLIC" >> space >> publicLiteral)
	where
		publicLiteral = do
			publicIDLiteral
			space
			systemLiteral

declSep :: P ()
declSep = space <|> (exact '%' *> void name <* exact ';')

elementDecl :: P ()
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

-- attributeListDecl :: P ()
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

-- entityDecl :: P ()
-- entityDecl = undefined

-- notationDecl :: P ()
-- notationDecl = undefined

markupDecl :: P ()
markupDecl =
	msum [elementDecl,
	      -- attributeListDecl,
	      -- entityDecl,
	      -- notationDecl,
	      void instruction,
	      void comment]

intSubSet :: P ()
intSubSet =
	() <$ many (markupDecl <|> declSep)

docType :: P ()
docType = do
	string "<!DOCTYPE"
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

contents :: P [Content]
contents =
	many (msum [Text <$> (charData <|> cData),
	            uncurry Open <$> openTag,
	            uncurry Empty <$> emptyTag,
	            Close <$> closeTag,
	            uncurry Instruction <$> instruction,
	            DocType <$ docType,
	            Comment <$> comment])

endReached :: P ()
endReached =
	inspect $ \ src ->
		if T.null src then
			Just ((), src)
		else
			Nothing

xml :: P [Content]
xml = contents <* endReached
