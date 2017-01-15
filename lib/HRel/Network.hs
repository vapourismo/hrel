{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module HRel.Network (
	httpRequest,
	httpDownload
) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Control.Exception

import           Data.Conduit

import qualified Data.ByteString as B

import           Network.HTTP.Client
import           Network.HTTP.Types

-- | Perform a HTTP request, yield chunks of its response body.
httpRequest :: (MonadResource m) => Manager -> Request -> Producer m B.ByteString
httpRequest mgr interimReq =
	bracketP (responseOpen req mgr) responseClose (yieldAll . responseBody)
	where
		req =
			interimReq {
				requestHeaders = (hUserAgent, "hrel-haskell/0.0.0") : requestHeaders interimReq
			}

		yieldAll reader = do
			chunk <- liftIO reader
			unless (B.null chunk) $ do
				yield chunk
				yieldAll reader

-- | Perform a HTTP request, return chunks of its response body.
httpDownload :: Manager -> Request -> IO [B.ByteString]
httpDownload mgr interimReq =
	bracket (responseOpen req mgr) responseClose (brConsume . responseBody)
	where
		req =
			interimReq {
				requestHeaders = (hUserAgent, "hrel-haskell/0.0.0") : requestHeaders interimReq
			}
