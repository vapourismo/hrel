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

	Source (..),
	single,
	exact,
	range
) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Applicative
import           Data.Bifunctor
import           Data.Semigroup

import           Data.Char
import           Data.Word
import           Data.List (isPrefixOf)
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

class Source s a | s -> a where
	uncons :: s -> Maybe (a, s)

	split :: Int -> s -> Maybe (s, s)

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

exact :: (Source s a, Eq a) => a -> Parser s a
exact x =
	validate single $ \ y ->
		if y == x then
			Just x
		else
			Nothing

range :: (Source s a, Ord a) => a -> a -> Parser s a
range lower upper =
	validate single $ \ y ->
		if lower <= y && y <= upper then
			Just y
		else
			Nothing
