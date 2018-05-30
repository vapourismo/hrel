{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}

module HRel.Network
    ( RequestError (..)
    , requestResponseBody )
where

import Control.Exception      (handle)
import Control.Monad.Except   (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO (..))

import qualified Data.ByteString as ByteString

import Network.HTTP.Client
import Network.HTTP.Types

-- | Error that occurs when performing a 'Request'
data RequestError
    = BadResponseStatus (Response ())
    | RequestException HttpException
    deriving Show

-- | Perform an HTTP request and retrieve the response body if successful.
requestResponseBody
    :: (MonadError RequestError m, MonadIO m)
    => Manager
    -> Request
    -> m ByteString.ByteString
requestResponseBody manager baseRequest =
    liftIO (handleException (withResponse request manager handleResponse)) >>= liftEither
    where
        userAgent = (hUserAgent, "hrel/3")

        request = baseRequest {requestHeaders = userAgent : requestHeaders baseRequest}

        handleResponse response =
            case responseStatus response of
                Status 200 _ -> Right . ByteString.concat <$> brConsume (responseBody response)
                _            -> pure (Left (BadResponseStatus (() <$ response)))

        handleException = handle (pure . Left . RequestException)
