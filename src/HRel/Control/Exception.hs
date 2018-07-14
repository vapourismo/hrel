{-# OPTIONS -Wno-unused-top-binds #-}

{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HRel.Control.Exception
    ( Throws
    , throw
    , catch
    , handle
    , try

      -- * Re-exports
    , Exception (..)
    , Catch.MonadCatch
    , Catch.MonadThrow
    )
where

import GHC.Exts (Proxy#, proxy#)

import Data.Coerce (coerce)

import           Control.Exception   (Exception)
import qualified Control.Monad.Catch as Catch

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
try :: forall e a m. (Catch.MonadCatch m, Exception e) => (Throws e => m a) -> m (Either e a)
try action =
    Catch.try (removeAnnotation (proxy# :: Proxy# e) action)
