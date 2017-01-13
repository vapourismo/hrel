module HRel.Parser (
	Result (..),
	Parser (..),
	parse,
	feed,

	satisfy,
	anyChar,
	char,
	string,

	skipWhile,
	takeWhile,
	takeWhile1,
	sepBy1,
	manyTill,
	endOfInput,

	debugP
) where

import           Debug.Trace

import           Prelude hiding (takeWhile)

import           Control.Monad
import           Control.Applicative

import qualified Data.Text as T
import           Text.Show.Functions ()

data Result a
	= Complete T.Text a
	| Incomplete (T.Text -> Result a)
	| Error T.Text
	deriving (Show)

instance Functor Result where
	fmap f (Complete input x) = Complete input (f x)
	fmap f (Incomplete g)     = Incomplete (fmap f . g)
	fmap _ (Error input)      = Error input

	{-# INLINE fmap #-}

newtype Parser a = Parser {
	runParser :: T.Text -> Result a
}

parse :: Parser a -> T.Text -> Result a
parse = runParser

{-# INLINE parse #-}

feed :: Result a -> T.Text -> Result a
feed (Incomplete cont) input = cont input
feed x                 _     = x

{-# INLINE feed #-}

instance Functor Parser where
	fmap f (Parser p) =
		Parser (\ input -> fmap f (p input))

	{-# INLINE fmap #-}

instance Applicative Parser where
	pure x = Parser (\ input -> Complete input x)

	{-# INLINE pure #-}

	Parser pf <*> Parser px =
		Parser (\ input -> go (pf input))
		where
			go result =
				case result of
					Complete rest f -> fmap f (px rest)
					Incomplete cont -> Incomplete (go . cont)
					Error rest      -> Error rest

	{-# INLINE (<*>) #-}

instance Monad Parser where
	Parser px >>= f =
		Parser (\ input -> go (px input))
		where
			go result =
				case result of
					Complete rest x -> runParser (f x) rest
					Incomplete cont -> Incomplete (go . cont)
					Error rest      -> Error rest

	{-# INLINE (>>=) #-}

instance Alternative Parser where
	empty = Parser Error

	{-# INLINE empty #-}

	Parser pl <|> Parser pr =
		Parser (go pl pr)
		where
			go l r input = altResult (l input) (r input)

			altResult (Incomplete cl) (Incomplete cr) = Incomplete (go cl cr)
			altResult (Error _)       r               = r
			altResult l               _               = l

	{-# INLINE (<|>) #-}

instance MonadPlus Parser

debugP :: (Show a) => String -> Parser a -> Parser a
debugP title (Parser p) =
	Parser (go p)
	where
		go f input =
			trace (title ++ ": receives " ++ show input)
			      (maskResult (f input))

		maskResult result =
			case result of
				Complete rest x ->
					trace (title ++ ": yields " ++ show x ++ "\n" ++
					       title ++ ": left over " ++ show rest)
					      result

				Incomplete cont ->
					trace (title ++ ": incomplete") $
						Incomplete (go cont)

				Error rest ->
					trace (title ++ ": errored " ++ show rest) result

continueWith :: (T.Text -> Result a) -> Result a
continueWith cont =
	Incomplete $ \ input ->
		if T.null input then
			Error input
		else
			cont input

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
	Parser go
	where
		go input =
			case T.uncons input of
				Nothing           -> continueWith go
				Just (h, t) | f h -> Complete t h
				_                 -> Error input

{-# INLINE satisfy #-}

char :: Char -> Parser Char
char c = satisfy (== c)

{-# INLINE char #-}

anyChar :: Parser Char
anyChar = satisfy (const True)

{-# INLINE anyChar #-}

string :: T.Text -> Parser T.Text
string t =
	Parser (go t)
	where
		go text input
			| T.null text                 = Complete input t
			| T.null input                = continueWith (go text)
			| T.head text == T.head input = go (T.tail text) (T.tail input)
			| otherwise                   = Error input

{-# INLINE string #-}

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile cond =
	Parser go
	where
		checkMore rest
			| T.null rest = Incomplete more
			| otherwise   = Complete rest ()

		go input
			| T.null input = Incomplete more
			| otherwise    = checkMore (T.dropWhile cond input)

		more input
			| T.null input = Complete input ()
			| otherwise    = checkMore (T.dropWhile cond input)

{-# INLINE skipWhile #-}

takeWhile :: (Char -> Bool) -> Parser T.Text
takeWhile cond =
	Parser go
	where
		checkMore (start, rest)
			| T.null rest = Incomplete (more start)
			| otherwise   = Complete rest start

		go input
			| T.null input = Incomplete (more T.empty)
			| otherwise    = checkMore (T.span cond input)

		more captured input
			| T.null input = Complete input captured
			| otherwise    = T.append captured <$> checkMore (T.span cond input)

{-# INLINE takeWhile #-}

takeWhile1 :: (Char -> Bool) -> Parser T.Text
takeWhile1 cond =
	Parser (runParser (takeWhile cond) >>= go)
	where
		go result input =
			case result of
				Incomplete cont         -> Incomplete (cont >>= go)
				Complete _ x | T.null x -> Error input
				x                       -> x

{-# INLINE takeWhile1 #-}

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 x s =
	(:) <$> x <*> some (s >> x)

{-# INLINE sepBy1 #-}

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill x y =
	([] <$ y) <|> ((:) <$> x <*> manyTill x y)

{-# INLINE manyTill #-}

endOfInput :: Parser ()
endOfInput =
	Parser $ \ input ->
		if T.null input then
			Error input
		else
			Complete input ()

{-# INLINE endOfInput #-}
