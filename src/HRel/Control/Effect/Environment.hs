{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module HRel.Control.Effect.Environment
    ( HasEnvironment (..)
    , asks
    )
where

import Control.Monad.Morph  (MFunctor (hoist), MonadTrans (lift))
import Control.Monad.Reader (ReaderT (..), mapReaderT)

import Data.Type.Equality (type (==))

{-# ANN module "HLint: ignore Use asks" #-}

-- | A functor @f@ that provides an environment @e@
class Functor f => HasEnvironment e f where
    ask :: f e

    local :: (e -> e) -> f a -> f a

    reader :: (e -> a) -> f a
    reader f = f <$> ask

-- | Alias for 'reader'.
asks :: HasEnvironment e f => (e -> a) -> f a
asks = reader

-- | Exists sole to avoid having to use overlapping instances
class b ~ (e == s) => Readerer (b :: Bool) e s (f :: * -> *) where
    askReaderT :: ReaderT s f e

    localReaderT :: (e -> e) -> ReaderT s f a -> ReaderT s f a

instance ((e == e) ~ 'True, Applicative f) => Readerer 'True e e f where
    askReaderT = ReaderT pure

    localReaderT f (ReaderT cont) = ReaderT (cont . f)

instance ((e == s) ~ 'False, HasEnvironment e f) => Readerer 'False e s f where
    askReaderT = ReaderT (const ask)

    localReaderT f = mapReaderT (local f)

instance (Functor f, Readerer (e == s) e s f) => HasEnvironment e (ReaderT s f) where
    ask = askReaderT

    local = localReaderT

instance
    {-# OVERLAPPABLE #-}
    ( HasEnvironment e f
    , Monad f
    , MonadTrans t
    , MFunctor t
    , Functor (t f)
    )
    => HasEnvironment e (t f)
    where
        ask = lift ask

        local f = hoist (local f)
