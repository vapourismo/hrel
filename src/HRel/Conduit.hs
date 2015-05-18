module HRel.Conduit (
	-- * HTTP related
	FetchT,
	request,
	fetch,
	fetchGZipped,

	-- * Markup
	markup,
) where

import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Reader

import Data.Conduit
import qualified Data.Conduit.List as C

import Text.StringLike

import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as Z

import Network.HTTP.Types
import Network.HTTP.Client

import HRel.Markup

-- | Transformer used to perform HTTP "Request"s.
type FetchT = ReaderT Manager

-- | Generate a request.
request :: (MonadThrow m) => String -> Source m Request
request = parseUrl >=> yield

-- | Perform a request and retrieve the result body.
fetch :: (MonadIO m) => Conduit Request (FetchT m) BL.ByteString
fetch =
	C.mapM $ \ req -> do
		mgr <- ask
		liftIO $ handle (const (pure BL.empty) :: HttpException -> IO BL.ByteString) $
			flip fmap (httpLbs req mgr) $ \ res ->
				case responseStatus res of
					Status 200 _ -> responseBody res
					Status _   _ -> BL.empty

-- | Similiar to "fetch" but decompresses the result (independent from body compression).
fetchGZipped :: (MonadIO m) => Conduit Request (FetchT m) BL.ByteString
fetchGZipped =
	fetch =$= C.map Z.decompress

-- | Process incoming "StringLike" values and parse them using a "NodeFilterT".
markup :: (StringLike t, Monad m) => NodeFilterT t m a -> Conduit t m a
markup nf =
	C.mapMaybeM (runNodeFilterT nf . fromMarkup')
