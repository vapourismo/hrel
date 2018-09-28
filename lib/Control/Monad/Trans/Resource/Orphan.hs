{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -Wno-orphans #-}

module Control.Monad.Trans.Resource.Orphan () where

import Control.Monad.Base                    (MonadBase (..))
import Control.Monad.Trans.Control           (MonadBaseControl (..))
import Control.Monad.Trans.Resource.Internal (ResourceT (..))

instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase action = ResourceT (const (liftBase action))

instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
    type StM (ResourceT m) a = StM m a

    liftBaseWith action = ResourceT $ \ioRef -> liftBaseWith $ \runInner ->
        action (\ (ResourceT inner) -> runInner (inner ioRef))

    restoreM state = ResourceT (const (restoreM state))
