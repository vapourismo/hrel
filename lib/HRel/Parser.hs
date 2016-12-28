module HRel.Parser (
	Parser (..),
	inspect,
	save,
	mustFail,
	validate,
	excludeFrom,
	sepBy
) where

import           Control.Monad
import           Control.Applicative
import           Data.Semigroup

newtype Parser s a = Parser { runParser :: s -> (Maybe a, s) }

instance Functor (Parser s) where
	fmap f (Parser producer) =
		Parser $ \ src ->
			let (x, src') = producer src in (f <$> x, src')

	{-# INLINE fmap #-}

instance Applicative (Parser s) where
	pure x = Parser (\ src -> (Just x, src))

	{-# INLINE pure #-}

	fp <*> pp = fp >>= (<$> pp)

	{-# INLINE (<*>) #-}

instance Monad (Parser s) where
	Parser producer >>= action =
		Parser $ \ src ->
			case producer src of
				(Just x, src') -> runParser (action x) src'
				(_, src')      -> (Nothing, src')

	{-# INLINE (>>=) #-}

instance Alternative (Parser s) where
	empty = Parser ((,) Nothing)

	Parser lhs <|> Parser rhs =
		Parser (\ src -> alt (lhs src) (rhs src))
		where
			alt lhs@(Just _, _) _ = lhs
			alt _ rhs             = rhs

	{-# INLINE (<|>) #-}

instance MonadPlus (Parser s)

instance Monoid (Parser s a) where
	mempty = mzero
	mappend = mplus

instance Semigroup (Parser s a)

inspect :: (s -> Maybe (a, s)) -> Parser s a
inspect f =
	Parser $ \ src ->
		case f src of
			Just (x, src') -> (Just x, src')
			_              -> (Nothing, src)

save :: Parser s (Parser s ())
save =
	Parser (\ src -> (Just (restore src), src))
	where
		restore src =
			Parser (const (Just (), src))

mustFail :: Parser s a -> Parser s ()
mustFail parser = do
	restore <- save
	result <- (False <$ parser) <|> pure True
	restore
	guard result

validate :: Parser s a -> (a -> Bool) -> Parser s a
validate parser pred = do
	restore <- save
	result <- parser

	if pred result then
		pure result
	else do
		restore
		mzero

excludeFrom :: Parser s b -> Parser s a -> Parser s a
excludeFrom a b = do
	mustFail a
	b

sepBy :: Parser s a -> Parser s b -> Parser s [a]
sepBy elem sep =
	((:) <$> elem <*> many (sep >> elem)) <|> pure []
