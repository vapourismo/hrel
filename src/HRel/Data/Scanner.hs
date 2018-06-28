{-# LANGUAGE LambdaCase #-}

module HRel.Data.Scanner
    ( Scanner
    , runScanner
    , scannerConduit
    , await
    , pull
    , feed
    , seal )
where

import Control.Applicative
import Control.Monad

import qualified Data.Conduit    as Conduit
import           Data.Profunctor

-- | Scanner
data Scanner i a
    = Fail
    | Pure a
    | With (Maybe i -> Scanner i a)

instance Functor (Scanner i) where
    fmap _ Fail          = Fail
    fmap f (Pure x)      = Pure (f x)
    fmap f (With handle) = With (fmap f . handle)

    {-# INLINEABLE fmap #-}

instance Profunctor Scanner where
    dimap left right = \case
        Fail         -> Fail
        Pure value   -> Pure (right value)
        With handler -> With (dimap left right . handler . fmap left)

    {-# INLINEABLE dimap #-}

instance Applicative (Scanner i) where
    pure = Pure

    {-# INLINE pure #-}

    Fail     <*> _        = Fail
    _        <*> Fail     = Fail
    Pure lhs <*> rhs      = lhs <$> rhs
    With lhs <*> With rhs = With (\ input -> lhs input <*> rhs input)
    With lhs <*> rhs      = With (\ input -> lhs input <*> rhs)

    {-# INLINEABLE (<*>) #-}

instance Alternative (Scanner i) where
    empty = Fail

    {-# INLINE empty #-}

    Fail     <|> rhs      = rhs
    lhs      <|> Fail     = lhs
    Pure lhs <|> _        = Pure lhs
    With lhs <|> With rhs = With (\ input -> lhs input <|> rhs input)
    With lhs <|> rhs      = With (\ input -> lhs input <|> rhs)

    {-# INLINEABLE (<|>) #-}

    many parser =
        await >>= \case
            Just input -> feed input (some parser) <|> pure []
            Nothing    -> pure []

    some parser = do
        value <- parser
        (value :) <$> many parser

instance Monad (Scanner i) where
    Fail        >>= _ = Fail
    Pure x      >>= f = f x
    With handle >>= f = With (handle >=> f)

    {-# INLINE (>>=) #-}

instance MonadPlus (Scanner i)

-- | Execute the 'Scanner'.
runScanner :: Monad m => m (Maybe i) -> Scanner i a -> m (Maybe a)
runScanner await =
    evaluate
    where
        evaluate Fail            = pure Nothing
        evaluate (Pure x)        = pure (Just x)
        evaluate (With handler)  = await >>= evaluate . handler

-- | Execute the 'Scanner' as a 'Conduit.ConduitT'.
scannerConduit
    :: Monad m
    => Scanner i a
    -> Conduit.ConduitT i o m (Maybe a)
scannerConduit = runScanner Conduit.await

{-# INLINE scannerConduit #-}

-- | Prevent the 'Scanner' from receiving more inputs.
seal :: Scanner i a -> Scanner i a
seal = \case
    With handler -> seal (handler Nothing)
    other        -> other

-- | Await a new input.
await :: Scanner i (Maybe i)
await = With Pure

{-# INLINE await #-}

-- | Pull a new input. Fails if there are no more inputs.
pull :: Scanner i i
pull = With (maybe Fail Pure)

{-# INLINE pull #-}

-- | Feed a 'Scanner' a custom input.
feed :: i -> Scanner i a -> Scanner i a
feed input = \case
    With handler -> handler (Just input)
    other        -> other
