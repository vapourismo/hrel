module HRel.Fetch (
	FetchT,
	runFetchT
) where

import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Network.HTTP.Client

-- | Transformer used to perform HTTP "Request"s.
type FetchT = ReaderT Manager

-- | Perform the requested operations using a manager.
runFetchT :: (MonadIO m, MonadMask m) => FetchT m a -> ManagerSettings -> m a
runFetchT f ms =
	bracket (liftIO (newManager ms)) (liftIO . closeManager) (runReaderT f)
