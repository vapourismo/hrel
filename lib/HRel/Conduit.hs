{-# LANGUAGE RecordWildCards #-}

module HRel.Conduit (
	-- * Utilities
	request,
	fetch,
	fetchGZipped,
	markup,

	-- * Re-exports
	module Data.Conduit
) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans

import qualified Codec.Compression.GZip as Z

import           Data.Conduit
import qualified Data.Conduit.List      as C
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL

import           Text.StringLike

import           Network.HTTP.Client
import           Network.HTTP.Types

import           HRel.Markup

-- | Generate a request.
request :: (MonadThrow m) => String -> Conduit i m Request
request =
	parseUrl >=> yield

-- | Execute request and the response body.
fetchLazy :: (MonadIO m) => Manager -> Conduit Request m BL.ByteString
fetchLazy mgr =
	C.mapMaybeM $ \ req -> do
		liftIO $ handle (\ (SomeException _) -> pure Nothing) $ do
			res <- httpLbs req mgr
			pure $
				if responseStatus res == status200 then
					Just (responseBody res)
				else
					Nothing

-- | Perform a request and retrieve the result body.
fetch :: (MonadIO m) => Manager -> Conduit Request m B.ByteString
fetch mgr =
	fetchLazy mgr =$= C.map BL.toStrict

-- | Similiar to "fetch" but decompresses the result (independent from body compression).
fetchGZipped :: (MonadIO m) => Manager -> Conduit Request m B.ByteString
fetchGZipped mgr =
	fetchLazy mgr =$= C.map (BL.toStrict . Z.decompress)

-- | Process incoming "StringLike" values and parse them using a "NodeFilterT".
markup :: (Monad m, StringLike t) => NodeFilterT t m a -> ConduitM t a m ()
markup nf =
	C.mapMaybeM (runNodeFilterT nf . fromMarkup')
