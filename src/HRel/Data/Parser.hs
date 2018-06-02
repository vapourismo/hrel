{-# LANGUAGE LambdaCase #-}

module HRel.Data.Parser
    ( Parser (..)
    , terminate
    , seal
    , feed
    , await
    , pull
    , embed )
where

import Control.Applicative
import Control.Monad

-- | Parser
data Parser i a
    = Fail
    | Pure a
    | With (Maybe i -> Parser i a)

instance Functor (Parser i) where
    fmap _ Fail           = Fail
    fmap f (Pure x)       = Pure (f x)
    fmap f (With handler) = With (fmap f . handler)

instance Applicative (Parser i) where
    pure = Pure

    Fail     <*> _        = Fail
    _        <*> Fail     = Fail
    Pure f   <*> rhs      = f <$> rhs
    lhs      <*> Pure x   = ($ x) <$> lhs
    With lhs <*> With rhs = With (\ input -> lhs input <*> rhs input)

instance Alternative (Parser i) where
    empty = Fail

    Fail         <|> rhs      = rhs
    lhs          <|> Fail     = lhs
    Pure x       <|> _        = Pure x
    With lhs     <|> With rhs = With (\ input -> lhs input <|> rhs input)
    With handler <|> rhs      = With (\ input -> handler input <|> rhs)

    many parser =
        await >>= \case
            Just input -> feed input (some parser) <|> pure []
            Nothing    -> pure []

    some parser = do
        value <- parser
        (value :) <$> many parser

instance Monad (Parser i) where
    Fail         >>= _ = Fail
    Pure x       >>= f = f x
    With handler >>= f = With (\ i -> handler i >>= f)

    fail _ = Fail

instance MonadPlus (Parser i)

-- | Terminate a 'Parser'.
terminate :: Parser i a -> Maybe a
terminate Fail           = Nothing
terminate (Pure x)       = Just x
terminate (With handler) = terminate (handler Nothing)

-- | Force the 'Parser' to not accept more input.
seal :: Parser i a -> Parser i a
seal parser = maybe Fail Pure (terminate parser)

-- | Feed input to a 'Parser'.
feed :: i -> Parser i a -> Parser i a
feed input (With handler) = handler (Just input)
feed _     other          = other

-- | Await input.
await :: Parser i (Maybe i)
await = With Pure

-- | Pull input.
pull :: Parser i i
pull = With (maybe Fail Pure)

-- | Embed the parser into a 'Monad'.
embed :: Monad m => m (Maybe i) -> (a -> m ()) -> m () -> Parser i a -> m ()
embed get put error baseParser =
    withParser baseParser
    where
        withParser = \case
            Fail -> error

            Pure x -> do
                put x
                withParser baseParser

            With handler ->
                get >>= \case
                    Nothing -> maybe error put (terminate (handler Nothing))
                    other   -> withParser (handler other)
