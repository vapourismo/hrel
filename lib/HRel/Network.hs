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
httpRequest :: (MonadResource m) => Manager -> String -> Producer m B.ByteString
httpRequest mgr url = do
	interimReq <- parseRequest url
	let req =
		interimReq {
			requestHeaders = [(hUserAgent, "hrel-haskell/0.0.0")]
		}

	bracketP (responseOpen req mgr) responseClose (yieldAll . responseBody)
	where
		yieldAll reader = do
			chunk <- liftIO reader
			unless (B.null chunk) $ do
				yield chunk
				yieldAll reader

-- | Perform a HTTP request, return chunks of its response body.
httpDownload :: Manager -> String -> IO [B.ByteString]
httpDownload mgr url = do
	interimReq <- parseRequest url
	let req =
		interimReq {
			requestHeaders = [(hUserAgent, "hrel-haskell/0.0.0")]
		}

	bracket (responseOpen req mgr) responseClose (brConsume . responseBody)
