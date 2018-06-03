{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Network
    ( RequestError (..)
    , requestResponseBody
    , requestConduit )
where

import Control.Monad.Catch          (handle)
import Control.Monad.Except         (MonadError, liftEither, throwError)
import Control.Monad.Trans          (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, liftResourceT)

import qualified Data.ByteString as ByteString
import           Data.Conduit    (ConduitT, transPipe)

import Network.HTTP.Client
    ( HttpException
    , Manager
    , Request
    , Response (..)
    , brConsume
    , requestHeaders
    , withResponse
    )
import Network.HTTP.Conduit (http)
import Network.HTTP.Types   (Status (..), hUserAgent)

-- | Error that occurs when performing a 'Request'
data RequestError
    = BadResponseStatus (Response ())
    | RequestException HttpException
    deriving Show

-- | Fix the 'Request'.
fixRequest :: Request -> Request
fixRequest baseRequest =
    baseRequest {requestHeaders = userAgent : requestHeaders baseRequest}
    where
        userAgent = (hUserAgent, "hrel/3")

-- | Perform an HTTP request and retrieve the response body if successful.
requestResponseBody
    :: (MonadError RequestError m, MonadIO m)
    => Manager
    -> Request
    -> m ByteString.ByteString
requestResponseBody manager baseRequest =
    liftIO (handleException (withResponse request manager handleResponse)) >>= liftEither
    where
        request = fixRequest baseRequest

        handleResponse response =
            case responseStatus response of
                Status 200 _ -> Right . ByteString.concat <$> brConsume (responseBody response)
                _            -> pure (Left (BadResponseStatus (() <$ response)))

        handleException = handle (pure . Left . RequestException)

-- | Perform HTTP request and yield chunks of the response body if successful.
requestConduit
    :: (MonadResource m, MonadError RequestError m)
    => Manager
    -> Request
    -> ConduitT i ByteString.ByteString m ()
requestConduit manager baseRequest = do
    response <-
        liftResourceT (handle (pure . Left . RequestException) (Right <$> http request manager))
        >>= liftEither
    case responseStatus response of
        Status 200 _ -> transPipe liftResourceT (responseBody response)
        _            -> throwError (BadResponseStatus (() <$ response))
    where
        request = fixRequest baseRequest
