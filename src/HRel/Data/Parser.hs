module HRel.Data.Parser
where

import Control.Applicative

data Parser i a
    = Fail
    | Pure a
    | With (i -> Parser i a)

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

instance Monad (Parser i) where
    Fail         >>= _ = Fail
    Pure x       >>= f = f x
    With handler >>= f = With (\ i -> handler i >>= f)

get :: Parser i i
get = With Pure

feed :: i -> Parser i a -> Parser i a
feed input (With handler) = handler input
feed _     other          = other
