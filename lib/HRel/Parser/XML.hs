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
			("", _) -> Nothing
			x       -> Just x

satisfyMany :: (Char -> Bool) -> P T.Text
satisfyMany cond =
	inspect (Just . T.span cond)

stringUntil :: T.Text -> (Char -> Bool) -> T.Text -> Maybe (T.Text, T.Text)
stringUntil term cond =
	action
	where
		action input =
			case T.break breaker input of
				(body, rest) | T.isPrefixOf (T.take 1 term) rest && not (T.isPrefixOf term rest) ->
					case action (T.tail rest) of
						Just (body', rest') -> Just (T.concat [body, T.take 1 term, body'], rest')
						Nothing             -> Just (T.snoc body (T.head term), T.tail rest)

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

xmlS :: P ()
xmlS =
	() <$ satisfyMany (\ c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

xmlCharEntity :: P T.Text
xmlCharEntity = do
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
			fromName <$> xmlName

clearEntities :: T.Text -> T.Text
clearEntities input =
	extractResult (runParser parser input)
	where
		parser = many (satisfySome (/= '&') <|> xmlCharEntity <|> (T.singleton <$> exact '&'))

		extractResult (Just body, rest) = T.concat (body ++ [rest])
		extractResult _                 = input

xmlName :: P T.Text
xmlName =
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

xmlCharData :: P T.Text
xmlCharData =
	clearEntities <$> satisfySome (/= '<')

xmlComment :: P T.Text
xmlComment = do
	string "<!--"
	inspect (stringUntil "-->" isXMLChar) <* string "-->"

xmlCData :: P T.Text
xmlCData = do
	string "<![CDATA["
	inspect (stringUntil "]]>" isXMLChar) <* string "]]>"

xmlOpenTag :: P (T.Text, [Attribute])
xmlOpenTag = do
	exact '<'
	(,) <$> xmlName
	    <*> many (xmlS >> xmlAttribute)
	    <*  xmlS
	    <*  exact '>'

xmlEmptyTag :: P (T.Text, [Attribute])
xmlEmptyTag = do
	exact '<'
	(,) <$> xmlName
	    <*> many (xmlS >> xmlAttribute)
	    <*  xmlS
	    <*  string "/>"

xmlCloseTag :: P T.Text
xmlCloseTag = do
	string "</"
	xmlName <* xmlS <* exact '>'

xmlAttValue :: P T.Text
xmlAttValue = do
	(exact '\'' *> satisfyMany (/= '\'') <* exact '\'')
	<|> (exact '"' *> satisfyMany (/= '"') <* exact '"')

type Attribute = (T.Text, T.Text)

xmlAttribute :: P Attribute
xmlAttribute =
	(,) <$> xmlName
	    <*  xmlS
	    <*  exact '='
	    <*  xmlS
	    <*> (clearEntities <$> xmlAttValue)

xmlInstruction :: P (T.Text, T.Text)
xmlInstruction = do
	string "<?"
	(,) <$> xmlName
	    <*  xmlS
	    <*> (T.strip <$> inspect (stringUntil "?>" isXMLChar))
	    <*  string "?>"

data Content
	= Text T.Text
	| Comment T.Text
	| Open T.Text [Attribute]
	| Close T.Text
	| Empty T.Text [Attribute]
	| Instruction T.Text T.Text
	deriving (Show, Eq, Ord)

xmlContent :: P [Content]
xmlContent =
	many (msum [Text <$> (xmlCharData <|> xmlCData),
	            uncurry Open <$> xmlOpenTag,
	            uncurry Empty <$> xmlEmptyTag,
	            uncurry Instruction <$> xmlInstruction,
	            Close <$> xmlCloseTag,
	            Comment <$> xmlComment])

endReached :: P ()
endReached =
	inspect $ \ src ->
		if T.null src then
			Just ((), src)
		else
			Nothing

xml :: P [Content]
xml = xmlContent <* endReached
