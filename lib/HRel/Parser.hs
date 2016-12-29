module HRel.Parser (
	Parser (..),
	inspect,
	save,
	mustFail,
	validate,
	excludeFrom,
	sepBy,
	sepBy1,
	sepBy2
) where

import           Control.Monad
import           Control.Applicative
import           Data.Semigroup
import           Data.Bifunctor

newtype Parser s a = Parser { runParser :: s -> Either s (a, s) }

instance Functor (Parser s) where
	fmap f (Parser producer) =
		Parser $ \ src ->
			first f <$> producer src

	{-# INLINE fmap #-}

instance Applicative (Parser s) where
	pure x = Parser (\ src -> Right (x, src))

	{-# INLINE pure #-}

	fp <*> pp = fp >>= (<$> pp)

	{-# INLINE (<*>) #-}

instance Monad (Parser s) where
	Parser producer >>= action =
		Parser $ \ src -> do
			(x, src') <- producer src
			runParser (action x) src'

	{-# INLINE (>>=) #-}

instance Alternative (Parser s) where
	empty = Parser Left

	Parser lhs <|> Parser rhs =
		Parser (\ src -> alt (lhs src) (rhs src))
		where
			alt lhs@(Right _) _ = lhs
			alt _ rhs           = rhs

	{-# INLINE (<|>) #-}

instance MonadPlus (Parser s)

instance Monoid (Parser s a) where
	mempty = mzero
	mappend = mplus

instance Semigroup (Parser s a)

inspect :: (s -> Maybe (a, s)) -> Parser s a
inspect f =
	Parser $ \ src ->
		maybe (Left src) Right (f src)

save :: Parser s (Parser s ())
save =
	Parser (\ src -> Right (restore src, src))
	where
		restore src =
			Parser (const (Right ((), src)))

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

sepBy1 :: Parser s a -> Parser s b -> Parser s [a]
sepBy1 elem sep =
	(:) <$> elem <*> many (sep >> elem)

sepBy2 :: Parser s a -> Parser s b -> Parser s [a]
sepBy2 elem sep =
	(:) <$> elem <*> some (sep >> elem)
