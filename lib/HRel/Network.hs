{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Network
    ( requestResponseBody
    , requestConduit )
where

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
    , checkResponse
    , requestHeaders
    , throwErrorStatusCodes
    , withResponse
    )
import Network.HTTP.Conduit (http)
import Network.HTTP.Types   (hUserAgent)

import HRel.Control.Exception

-- | Fix the 'Request'.
fixRequest :: Request -> Request
fixRequest baseRequest =
    baseRequest
        { requestHeaders = userAgent : requestHeaders baseRequest
        , checkResponse  = fixedCheckResponse
        }
    where
        userAgent = (hUserAgent, "hrel/3")

        fixedCheckResponse request response = do
            checkResponse baseRequest request response
            throwErrorStatusCodes request response

-- | Perform an HTTP request and retrieve the response body if successful.
requestResponseBody
    :: ( MonadIO m
       , MonadThrow m
       , Throws HttpException
       )
    => Manager
    -> Request
    -> m ByteString.ByteString
requestResponseBody manager baseRequest =
    liftIO $
        withResponse (fixRequest baseRequest) manager $ \ response ->
            ByteString.concat <$> brConsume (responseBody response)

-- | Perform HTTP request and yield chunks of the response body if successful.
requestConduit
    :: ( MonadResource m
       , MonadThrow m
       , Throws HttpException
       )
    => Manager
    -> Request
    -> ConduitT i ByteString.ByteString m ()
requestConduit manager baseRequest = do
    response <- liftResourceT (http request manager)
    transPipe liftResourceT (responseBody response)
    where
        request = fixRequest baseRequest
