{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module HRel.Control.Monad.Exception
    ( MonadException
    , throw
    , catch
    , try
    , bracket
    )
where

import qualified Control.Exception                   as Exception
import           Control.Monad.Base                  (MonadBase (..))
import           Control.Monad.Reader                (ReaderT)
import           Control.Monad.Trans.Control         (MonadBaseControl (..))
import           Control.Monad.Trans.Resource.Orphan ()

class MonadException m where
    -- | 'Exception.throw'
    throw :: Exception.Exception e => e -> m a

    default throw :: (Exception.Exception e, MonadBase IO m) => e -> m a
    throw e = liftBase (Exception.throw e)

    -- | 'Exception.catch'
    catch :: Exception.Exception e => m a -> (e -> m a) -> m a

    default catch :: (Exception.Exception e, MonadBaseControl IO m) => m a -> (e -> m a) -> m a
    catch action handle = do
        resultState <- liftBaseWith $ \runM ->
            Exception.catch (runM action) (runM . handle)
        restoreM resultState

    -- | 'Exception.try'
    try :: Exception.Exception e => m a -> m (Either e a)

    default try :: (Exception.Exception e, MonadBaseControl IO m) => m a -> m (Either e a)
    try action = do
        resultState <- liftBaseWith $ \runM ->
            Exception.try (runM action)

        case resultState of
            Left err    -> pure (Left err)
            Right state -> Right <$> restoreM state

    -- | 'Exception.bracket'
    bracket :: m a -> (a -> m b) -> (a -> m c) -> m c

    default bracket :: MonadBaseControl IO m => m a -> (a -> m b) -> (a -> m c) -> m c
    bracket start end inner = do
        resultState <- liftBaseWith $ \runM ->
            Exception.bracket
                (runM start)
                (\state -> runM (restoreM state >>= end))
                (\state -> runM (restoreM state >>= inner))

        restoreM resultState

instance MonadException IO

-- I don't know, man.
instance MonadBaseControl IO m => MonadException (ReaderT r m)

instance MonadBaseControl IO (t m) => MonadException (t m)
