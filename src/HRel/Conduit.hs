module HRel.Conduit (
	-- * Conduit
	HRelConduitM,
	HRelSource,
	HRelConduit,
	HRelSink,
	runHRelConduit,

	-- * Utilities
	request,
	fetch,
	fetchGZipped,
	markup,

	-- * Re-exports
	module Data.Conduit
) where

import Control.Monad.Reader

import Data.Void
import Text.StringLike

import qualified Codec.Compression.GZip as Z

import Data.Conduit
import qualified Data.Conduit.List as C

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Client
import Network.HTTP.Types

import HRel.Markup

data HRel = HRel Manager

-- | Conduit base monad
type HRelConduitM i o r = ConduitM i o (ReaderT HRel IO) r

-- | Source
type HRelSource o = HRelConduitM () o ()

-- | Conduit
type HRelConduit i o = HRelConduitM i o ()

-- | Sink
type HRelSink i r = HRelConduitM i Void r

-- | Execute a conduit sequence.
runHRelConduit :: Manager -> HRelSink () r -> IO r
runHRelConduit mgr sink =
	runReaderT (runConduit sink) (HRel mgr)

-- | Generate a request.
request :: String -> HRelSource Request
request =
	parseUrl >=> yield

-- | Execute request and the response body.
fetchLazy :: HRelConduit Request BL.ByteString
fetchLazy =
	C.mapMaybeM $ \ req -> do
		HRel mgr <- ask
		res <- liftIO (httpLbs req mgr)

		pure $
			if responseStatus res == status200 then
				Just (responseBody res)
			else
				Nothing

-- | Perform a request and retrieve the result body.
fetch :: HRelConduit Request B.ByteString
fetch =
	fetchLazy =$= C.map BL.toStrict

-- | Similiar to "fetch" but decompresses the result (independent from body compression).
fetchGZipped :: HRelConduit Request B.ByteString
fetchGZipped =
	fetchLazy =$= C.map (BL.toStrict . Z.decompress)

-- | Process incoming "StringLike" values and parse them using a "NodeFilterT".
markup :: (StringLike t) => NodeFilterT t IO a -> HRelConduit t a
markup nf =
	C.mapMaybeM (liftIO . runNodeFilterT nf . fromMarkup')
