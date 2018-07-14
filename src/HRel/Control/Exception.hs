{-# OPTIONS -Wno-unused-top-binds #-}

{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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

import GHC.Prim (coerce)

import           Control.Exception   (Exception)
import qualified Control.Monad.Catch as Catch

-- | Prevents implementation of 'Throws'
class ThrowsBrother e

type role ThrowsBrother representational

-- | An exception @e@ may be thrown
class ThrowsBrother e => Throws e

type role Throws representational

-- | Exists exclusively to strip the 'Throws' constraint
newtype Wrap e a = Wrap {unWrap :: Throws e => a}

-- | Only instance of the 'Throws' class
newtype Tau e = Tau e

instance ThrowsBrother (Tau e)

instance Throws (Tau e)

-- | Strip the 'Throws' constraint.
removeAnnotation :: forall e a p. p e -> (Throws e => a) -> a
removeAnnotation _ action =
    unWrap (coerce @(Wrap e a) @(Wrap (Tau e) a) (Wrap action))

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
    Catch.catch (removeAnnotation (id @e) action)

-- | Handle an 'Exception'.
handle
    :: forall e a m
    .  (Catch.MonadCatch m, Exception e)
    => (e -> m a)
    -> (Throws e => m a)
    -> m a
handle recover action =
    Catch.handle recover (removeAnnotation (id @e) action)

-- | Try and capture.
try :: forall e a m. (Catch.MonadCatch m, Exception e) => (Throws e => m a) -> m (Either e a)
try action =
    Catch.try (removeAnnotation (id @e) action)
