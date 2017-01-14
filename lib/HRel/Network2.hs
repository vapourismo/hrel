{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module HRel.Network2 (
	httpRequest
) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource

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
