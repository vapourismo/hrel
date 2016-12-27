{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FlexibleInstances,
             FunctionalDependencies #-}

module HRel.Parser (
	ParseError (..),
	Parser (..),
	inspect,
	save,
	mustFail,
	validate,
	excludeFrom,
	sepBy,

	Source (..),
	satisfy,
	single,
	exact,
	oneOf,
	notOneOf,
	range
) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Applicative
import           Data.Bifunctor
import           Data.Semigroup

import           Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

data ParseError
	= SourceDrained
	| CriteriaUnmet
	deriving (Show, Eq, Ord)

newtype Parser s a = Parser { runParser :: s -> Either ParseError (a, s) }

instance Functor (Parser s) where
	fmap f (Parser producer) =
		Parser (fmap (first f) <$> producer)

instance Applicative (Parser s) where
	pure x =
		Parser (\ src -> pure (x, src))

	Parser funcProducer <*> Parser paramProducer =
		Parser $ \ src -> do
			(func, src') <- funcProducer src
			(param, src'') <- paramProducer src'
			pure (func param, src'')

instance Monad (Parser s) where
	Parser producer >>= action =
		Parser $ \ src -> do
			(x, src') <- producer src
			runParser (action x) src'

altEither :: Either a b -> Either a b -> Either a b
altEither (Left _) b = b
altEither a _        = a

instance Alternative (Parser s) where
	empty = Parser (const (Left SourceDrained))

	Parser lhs <|> Parser rhs =
		Parser (\ src -> altEither (lhs src) (rhs src))

instance MonadPlus (Parser s)

instance Monoid (Parser s a) where
	mempty = empty
	mappend = (<|>)

instance Semigroup (Parser s a)

instance MonadError ParseError (Parser s) where
	throwError err =
		Parser (const (Left err))

	catchError (Parser producer) handler =
		Parser $ \ src ->
			case producer src of
				Left err -> runParser (handler err) src
				x        -> x

inspect :: (s -> Maybe (a, s)) -> Parser s a
inspect f =
	Parser (maybe (Left SourceDrained) Right . f)

save :: Parser s (Parser s ())
save =
	Parser $ \ src ->
		pure (Parser (const (pure ((), src))), src)

mustFail :: Parser s a -> Parser s ()
mustFail parser = do
	restore <- save
	result <- (True <$ parser) <|> pure False
	restore
	when result (throwError CriteriaUnmet)

validate :: Parser s a -> (a -> Maybe b) -> Parser s b
validate parser func = do
	restore <- save
	result <- parser
	maybe (restore >> throwError CriteriaUnmet) pure (func result)

excludeFrom :: Parser s b -> Parser s a -> Parser s a
excludeFrom a b = do
	mustFail a
	b

sepBy :: Parser s a -> Parser s b -> Parser s [a]
sepBy elem sep =
	((:) <$> elem <*> many (sep >> elem)) <|> pure []

class Source s a | s -> a where
	uncons :: s -> Maybe (a, s)

instance Source B.ByteString Word8 where
	uncons = B.uncons

instance Source BL.ByteString Word8 where
	uncons = BL.uncons

instance Source T.Text Char where
	uncons = T.uncons

instance Source TL.Text Char where
	uncons = TL.uncons

instance Source [a] a where
	uncons []       = Nothing
	uncons (x : xs) = Just (x, xs)

single :: (Source s a) => Parser s a
single =
	Parser (maybe (Left SourceDrained) Right . uncons)

satisfy :: (Source s a) => (a -> Bool) -> Parser s a
satisfy f =
	validate single $ \ x ->
		if f x then
			Just x
		else
			Nothing

exact :: (Source s a, Eq a) => a -> Parser s a
exact x = satisfy (== x)

oneOf :: (Source s a, Eq a) => [a] -> Parser s a
oneOf xs = satisfy (`elem` xs)

notOneOf :: (Source s a, Eq a) => [a] -> Parser s a
notOneOf xs =
	satisfy (not . (`elem` xs))

range :: (Source s a, Ord a) => a -> a -> Parser s a
range lower upper =
	satisfy (\ y -> lower <= y && y <= upper)
