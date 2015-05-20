{-# LANGUAGE FlexibleContexts #-}

module HRel.Conduit (
	-- * Conduits
	request,
	fetch,
	fetchGZipped,
	markup
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

-- | Generate a request.
request :: (MonadThrow m) => String -> Conduit i m Request
request = parseUrl >=> yield

-- | Perform a request and retrieve the result body.
fetch :: (MonadIO m) => Manager -> Conduit Request m BL.ByteString
fetch mgr =
	C.mapM $ \ req ->
		liftIO $ handle (const (pure BL.empty) :: HttpException -> IO BL.ByteString) $
			flip fmap (httpLbs req mgr) $ \ res ->
				case responseStatus res of
					Status 200 _ -> responseBody res
					Status _   _ -> BL.empty

-- | Similiar to "fetch" but decompresses the result (independent from body compression).
fetchGZipped :: (MonadIO m) => Manager -> Conduit Request m BL.ByteString
fetchGZipped mgr =
	fetch mgr =$= C.map Z.decompress

-- | Process incoming "StringLike" values and parse them using a "NodeFilterT".
markup :: (StringLike t, Monad m) => NodeFilterT t m a -> Conduit t m a
markup nf =
	C.mapMaybeM (runNodeFilterT nf . fromMarkup')
