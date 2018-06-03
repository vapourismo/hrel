{-# LANGUAGE LambdaCase #-}

module HRel.Data.Traversal
    ( TraversalT
    , runTraversalT
    , traverseConduit
    , await
    , pull
    , feed
    , seal )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import qualified Data.Conduit as Conduit

-- | Traversal over @i@ in @f@.
data TraversalT i f a
    = Fail
    | Pure a
    | Free (f (TraversalT i f a))
    | With (Maybe i -> TraversalT i f a)

instance Functor f => Functor (TraversalT i f) where
    fmap _ Fail          = Fail
    fmap f (Pure x)      = Pure (f x)
    fmap f (Free action) = Free (fmap f <$> action)
    fmap f (With handle) = With (fmap f . handle)

    {-# INLINE fmap #-}

instance Functor f => Applicative (TraversalT i f) where
    pure = Pure

    {-# INLINE pure #-}

    Fail     <*> _        = Fail
    _        <*> Fail     = Fail
    Pure lhs <*> rhs      = lhs <$> rhs
    Free lhs <*> rhs      = Free ((<*> rhs) <$> lhs)
    With lhs <*> With rhs = With (\ input -> lhs input <*> rhs input)
    With lhs <*> rhs      = With (\ input -> lhs input <*> rhs)

    {-# INLINE (<*>) #-}

instance Functor f => Alternative (TraversalT i f) where
    empty = Fail

    {-# INLINE empty #-}

    Fail     <|> rhs      = rhs
    lhs      <|> Fail     = lhs
    Pure lhs <|> _        = Pure lhs
    Free lhs <|> rhs      = Free ((<|> rhs) <$> lhs)
    With lhs <|> With rhs = With (\ input -> lhs input <|> rhs input)
    With lhs <|> rhs      = With (\ input -> lhs input <|> rhs)

    {-# INLINE (<|>) #-}

    many parser =
        await >>= \case
            Just input -> feed input (some parser) <|> pure []
            Nothing    -> pure []

    some parser = do
        value <- parser
        (value :) <$> many parser

instance Functor f => Monad (TraversalT i f) where
    Fail        >>= _ = Fail
    Pure x      >>= f = f x
    Free action >>= f = Free ((>>= f) <$> action)
    With handle >>= f = With (handle >=> f)

    {-# INLINE (>>=) #-}

instance Functor f => MonadPlus (TraversalT i f)

instance MonadTrans (TraversalT i) where
    lift = Free . fmap Pure

    {-# INLINE lift #-}

-- | Execute the 'TraversalT'.
runTraversalT :: Monad m => m (Maybe i) -> TraversalT i m a -> m (Maybe a)
runTraversalT await =
    evaluate
    where
        evaluate Fail            = pure Nothing
        evaluate (Pure x)        = pure (Just x)
        evaluate (Free continue) = continue >>= evaluate
        evaluate (With handler)  = await >>= evaluate . handler

-- | Execute the 'TraversalT' as a 'Conduit.ConduitT'.
traverseConduit
    :: Monad m
    => TraversalT i (Conduit.ConduitT i o m) a
    -> Conduit.ConduitT i o m (Maybe a)
traverseConduit = runTraversalT Conduit.await

{-# INLINE traverseConduit #-}

-- | Prevent the 'TraversalT' from receiving more inputs.
seal :: Functor f => TraversalT i f a -> TraversalT i f a
seal = \case
    Free action  -> Free (seal <$> action)
    With handler -> seal (handler Nothing)
    other        -> other

-- | Await a new input.
await :: TraversalT i f (Maybe i)
await = With Pure

{-# INLINE await #-}

-- | Pull a new input. Fails if there are no more inputs.
pull :: TraversalT i f i
pull = With (maybe Fail Pure)

{-# INLINE pull #-}

-- | Feed a 'TraversalT' a custom input.
feed :: Functor f => i -> TraversalT i f a -> TraversalT i f a
feed input = \case
    Free action  -> Free (feed input <$> action)
    With handler -> handler (Just input)
    other        -> other
