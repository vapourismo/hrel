{-# LANGUAGE RecordWildCards #-}

module HRel.Conduit (
	-- * Types
	HRel (..),
	HRelT,

	-- * Conduit
	HRelConduitM,
	HRelSource,
	HRelConduit,
	HRelSink,
	runHRelConduit,

	forkWorkerConduit,

	-- * Utilities
	request,
	fetch,
	fetchGZipped,
	markup,

	-- * Re-exports
	module Data.Conduit
) where

import           Control.Concurrent hiding (yield)
import           Control.Exception
import           Control.Monad.Reader

import qualified Codec.Compression.GZip as Z

import           Data.Void
import           Data.Conduit
import qualified Data.Conduit.List      as C
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL

import           Text.StringLike

import           Network.HTTP.Client
import           Network.HTTP.Types

import           HRel.Markup

data HRel = HRel Manager

type HRelT = ReaderT HRel IO

type HRelConduitM i o r = ConduitM i o HRelT r

-- | Source
type HRelSource o = HRelConduit () o

-- | Conduit
type HRelConduit i o = HRelConduitM i o ()

-- | Sink
type HRelSink i r = HRelConduitM i Void r

-- | Execute a conduit sequence.
runHRelConduit :: Manager -> HRelSink () r -> IO r
runHRelConduit mgr sink =
	runReaderT (runConduit sink) (HRel mgr)

-- | Generate a request.
request :: String -> HRelConduit i Request
request =
	parseUrl >=> yield

-- | Execute request and the response body.
fetchLazy :: HRelConduit Request BL.ByteString
fetchLazy =
	C.mapMaybeM $ \ req -> do
		HRel mgr <- ask
		liftIO $ handle (\ (SomeException _) -> pure Nothing) $ do
			res <- httpLbs req mgr
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
markup :: (StringLike t) => NodeFilterT t HRelT a -> HRelConduit t a
markup nf =
	C.mapMaybeM (runNodeFilterT nf . fromMarkup')

-- | Create a source for which takes its outputs from a channel.
sourceChan :: (MonadIO m) => Int -> Chan (Maybe a) -> Source m a
sourceChan num chan =
	when (num > 0) $ do
		msg <- liftIO (readChan chan)
		case msg of
			Nothing ->
				sourceChan (num - 1) chan

			Just x -> do
				yield x
				sourceChan num chan

-- | Create a sink for directs its inputs to a channel
sinkChan :: (MonadIO m) => Int -> Chan (Maybe a) -> Sink a m ()
sinkChan num chan = do
	msg <- await
	case msg of
		Nothing ->
			liftIO (replicateM_ num (writeChan chan Nothing))

		value -> do
			liftIO (writeChan chan value)
			sinkChan num chan

-- | Fork a worker thread which
forkWorkerConduit :: Int -> Manager -> HRelConduit i o -> IO (HRelSink i (), HRelSource o)
forkWorkerConduit num mgr conduit = do
	(input, output) <- (,) <$> newChan <*> newChan

	replicateM_ num $ forkIO $
		runHRelConduit mgr (sourceChan 1 input =$= conduit =$= sinkChan 1 output)

	pure (sinkChan num input, sourceChan num output)
