{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}

module HRel.Network (
	HttpError,

	HttpProducer,
	httpRequest,

	HttpIO,
	httpDownload,

	httpProbe
) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.Resource

import           Data.Conduit

import qualified Data.ByteString as B

import           Network.HTTP.Client
import           Network.HTTP.Types

import           HRel.Monad

-- | Error during one of the operations in this module
data HttpError = HttpError Request HttpException
	deriving (Show)

-- | Producer which may come across a 'HttpError'
type HttpProducer m o = forall i . HRelT HttpError (ConduitM i o) m ()

-- | Perform a HTTP request, yield chunks of its response body.
httpRequest :: (MonadCatch m, MonadResource m) => Manager -> Request -> HttpProducer m B.ByteString
httpRequest mgr interimReq =
	catch (bracketP (responseOpen req mgr) responseClose (yieldAll . responseBody))
	      (\ err -> throwError (HttpError req err))
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

-- | Operation which may result in a 'HttpError'
type HttpIO a = ExceptT HttpError IO a

-- | Perform a HTTP request, return chunks of its response body.
httpDownload :: Manager -> Request -> HttpIO [B.ByteString]
httpDownload mgr interimReq =
	catch (liftIO (bracket (responseOpen req mgr) responseClose (brConsume . responseBody)))
	      (\ err -> throwError (HttpError req err))
	where
		req =
			interimReq {
				requestHeaders = (hUserAgent, "hrel-haskell/0.0.0") : requestHeaders interimReq
			}

-- | Test if the request can potentially be performed.
httpProbe :: Manager -> Request -> IO Bool
httpProbe mgr interimReq =
	catch (True <$ (responseOpen req mgr >>= responseClose))
	      (\ (SomeException _) -> pure False)
	where
		req =
			interimReq {
				requestHeaders = (hUserAgent, "hrel-haskell/0.0.0") : requestHeaders interimReq
			}
