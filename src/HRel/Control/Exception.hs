{-# OPTIONS -Wno-unused-top-binds #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module HRel.Control.Exception
    ( Throws
    , throw
    , catch
    , handle
    , try
    , mapException
    , failOnException

    , Bomb (..)
    , SmallBomb

      -- * Re-exports
    , Exception
    , Catch.MonadCatch
    , Catch.MonadThrow
    , Catch.MonadMask
    )
where

import GHC.Exts (Proxy#, proxy#)

import Prelude hiding (fail)

import           Control.Exception   (Exception)
import qualified Control.Monad.Catch as Catch
import           Control.Monad.Fail  (MonadFail (fail))
import           Control.Monad.Trans (MonadTrans (lift))

import Data.Coerce (coerce)
import Data.Kind   (Constraint)

-- | Prevents implementation of 'Throws'
class ThrowsBrother e

type role ThrowsBrother representational

-- | An exception @e@ may be thrown
class ThrowsBrother e => Throws e

type role Throws representational

-- | Needed to coerce the dictionary for 'Throws' in the contravariant position
newtype Wrap e a = Wrap {unWrap :: Throws e => a}

-- | Only instance of the 'Throws' class
newtype Tau e = Tau e

instance ThrowsBrother (Tau e)

instance Throws (Tau e)

-- | Strip the 'Throws' constraint.
removeAnnotation :: forall e a. Proxy# e -> (Throws e => a) -> a
removeAnnotation _ action =
    unWrap (coerce (Wrap action :: Wrap e a) :: Wrap (Tau e) a)

-- | Redirect 'Exception' to 'MonadFail' interface.
failOnException
    :: forall e a m
    .  (Exception e, Catch.MonadCatch m, MonadFail m)
    => (Throws e => m a)
    -> m a
failOnException action =
    catch action (fail . show :: e -> m a)

-- | Throw an 'Exception'.
throw
    :: (Catch.MonadThrow m, Exception e)
    => e
    -> Throws e
    => m a
throw = Catch.throwM

-- | Catch an 'Exception'.
catch
    :: forall e a m
    .  (Catch.MonadCatch m, Exception e)
    => (Throws e => m a)
    -> (e -> m a)
    -> m a
catch action =
    Catch.catch (removeAnnotation (proxy# :: Proxy# e) action)

-- | Handle an 'Exception'.
handle
    :: forall e a m
    .  (Catch.MonadCatch m, Exception e)
    => (e -> m a)
    -> (Throws e => m a)
    -> m a
handle recover action =
    Catch.handle recover (removeAnnotation (proxy# :: Proxy# e) action)

-- | Try and capture.
try :: forall e a m
    .  (Catch.MonadCatch m, Exception e)
    => (Throws e => m a)
    -> m (Either e a)
try action =
    Catch.try (removeAnnotation (proxy# :: Proxy# e) action)

-- | Transform an 'Exception' when it occurs.
mapException
    :: forall e e' a m
    .  (Exception e, Exception e', Catch.MonadCatch m)
    => (e -> e')
    -> (Throws e => m a)
    -> (Throws e' => m a)
mapException map =
    handle (throw . map)

type family ThrowsMany (es :: [*]) :: Constraint where
    ThrowsMany '[]      = ()
    ThrowsMany '[e]     = Throws e
    ThrowsMany (e : es) = (Throws e, ThrowsMany es)

newtype Bomb es f a =
    Bomb {defuse :: ThrowsMany es => f a}
    deriving (Functor)

instance Applicative f => Applicative (Bomb es f) where
    pure x = Bomb (pure x)

    Bomb f <*> Bomb x = Bomb (f <*> x)

instance Monad f => Monad (Bomb es f) where
    Bomb m >>= f = Bomb (m >>= defuse . f)

instance MonadTrans (Bomb es) where
    lift = Bomb

type SmallBomb e = Bomb '[e]
