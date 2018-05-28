{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}

module HRel.Network
    ( performRequestConduit )
where

import Control.Monad.Except
import Control.Monad.Trans.Resource

import qualified Data.ByteString as ByteString
import           Data.Conduit

import Network.HTTP.Conduit
import Network.HTTP.Types   (Status (..), hUserAgent)

-- | Error that occurs when performing a 'Request'
data RequestError = forall b. BadResponseStatus (Response b)

-- | Perform a request and
performRequestConduit
    :: ( MonadError RequestError m
       , MonadResource m )
    => Manager
    -> Request
    -> ConduitT i ByteString.ByteString m ()
performRequestConduit manager baseRequest = do
    response <- lift (http request manager)
    case responseStatus response of
        Status 200 _ -> responseBody response
        _            -> throwError (BadResponseStatus response)
    where
        userAgent = (hUserAgent, "hrel/3")

        request =
            baseRequest {requestHeaders = userAgent : requestHeaders baseRequest}
