module HRel.Monad (
	HRelT,
	withHRelT,
	toHRelT,

	reportAndRecover,
	captureError,

	throwError,
	catchError
) where

import Control.Monad.Trans
import Control.Monad.Catch
import Control.Monad.Morph
import Control.Monad.Except

-- | HRel monad transformer
type HRelT e t m a = t (ExceptT e m) a

-- | Transform the underlying error type.
withHRelT :: (MFunctor t, Monad m) => (e1 -> e2) -> HRelT e1 t m a -> HRelT e2 t m a
withHRelT f =
	hoist (withExceptT f)

-- | Capture exceptions within the inner monad.
toHRelT :: (MFunctor t, MonadCatch m, Exception e) => t m a -> HRelT e t m a
toHRelT = hoist captureError

-- | Catch exceptions from the given action and store them in the new monad.
captureError :: (Monad m, MonadCatch (t m), MonadTrans t, Exception e, MonadError e (t m))
             => m a -> t m a
captureError action =
	catch (lift action) throwError

-- | Recover from errors. Also report them.
reportAndRecover :: (MonadIO m, MonadError e m, Monoid a) => (e -> IO b) -> m a -> m a
reportAndRecover report m =
	catchError m (\ e -> mempty <$ liftIO (report e))
