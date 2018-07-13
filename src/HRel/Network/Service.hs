{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HRel.Network.Service
    ( serveRequests

    , Service
    , IntroException (..)
    , introduce
    , RequestException (..)
    , request )
where

import GHC.Generics (Generic)

import Control.Exception   (SomeException (..))
import Control.Monad       (forever)
import Control.Monad.Trans (MonadIO)

import           Data.Aeson             (FromJSON (..), ToJSON (..))
import qualified Data.Binary            as Binary
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.Text.Encoding     as TextEncoding
import           Data.Typeable          (TypeRep, Typeable, typeRep)

import HRel.Control.Exception
import HRel.Network.ZMQ

newtype JsonBinary a = JsonBinary a
    deriving (Show, Eq)

instance Binary.Binary a => ToJSON (JsonBinary a) where
    toJSON (JsonBinary value) =
        toJSON
        $ TextEncoding.decodeUtf8
        $ Base64.encode
        $ LazyByteString.toStrict
        $ Binary.encode value

instance Binary.Binary a => FromJSON (JsonBinary a) where
    parseJSON value = do
        base64Bytes <- TextEncoding.encodeUtf8 <$> parseJSON value
        bytes       <- either fail pure (Base64.decode base64Bytes)

        case Binary.decodeOrFail (LazyByteString.fromStrict bytes) of
            Left (_, _, message) -> fail message
            Right (_, _, result) -> pure (JsonBinary result)

-- | Service request
data Request a
    = TypeCheck (JsonBinary TypeRep) (JsonBinary TypeRep)
    | Request a
    deriving (Show, Eq, Functor, Generic)

instance FromJSON a => FromJSON (Request a)

instance ToJSON a => ToJSON (Request a)

-- | Service response
data Response a
    = InvalidRequest String
    | UncaughtException String
    | TypeCheckFailed (JsonBinary TypeRep) (JsonBinary TypeRep)
    | TypeCheckOk
    | Response a
    deriving (Show, Eq, Functor, Generic)

instance FromJSON a => FromJSON (Response a)

instance ToJSON a => ToJSON (Response a)

-- | Serve requests from clients.
serveRequests
    :: forall i o m a
    .  ( Typeable i
       , FromJSON i
       , Typeable o
       , ToJSON o
       , MonadIO m
       , MonadCatch m )
    => Socket Rep
    -> (i -> m o)
    -> m a
serveRequests socket handleRequest =
    forever $ do
        response <- receiveJson socket >>= \case
            Left errorMessage ->
                pure (InvalidRequest errorMessage)

            Right (TypeCheck (JsonBinary inputType) (JsonBinary outputType)) ->
                if inputType == realInputType && outputType == realOutputType then
                    pure TypeCheckOk
                else
                    pure (TypeCheckFailed (JsonBinary realInputType) (JsonBinary realOutputType))

            Right (Request request) ->
                catch (Response <$> handleRequest request) $ \ (SomeException e) ->
                    pure (UncaughtException (show e))

        sendJson socket response
    where
        realInputType  = typeRep (id @i)
        realOutputType = typeRep (id @o)


-- | Service
newtype Service i o = Service (Socket Req)

-- | Exception that may occur during introduction
data IntroException
    = IntroResponseParseError String
    | IntroUnexpectedResponse (Response ())
    deriving (Show, Eq)

instance Exception IntroException

-- | Introduce to the remote 'Service'.
introduce
    :: forall i o m
    .  ( Typeable i
       , Typeable o
       , MonadIO m
       , MonadThrow m
       , Throws IntroException )
    => Socket Req
    -> m (Service i o)
introduce socket = do
    sendJson socket
        (TypeCheck @()
            (JsonBinary (typeRep (id @i)))
            (JsonBinary (typeRep (id @o))))

    receiveJson socket >>= \case
        Right TypeCheckOk -> pure (Service socket)
        Right response    -> throw (IntroUnexpectedResponse response)
        Left error        -> throw (IntroResponseParseError error)

-- | Exception that may occur during request
data RequestException
    = RequestResponseParseError String
    | RequestUnexpectedResponse (Response ())
    | RequestRemoteException String
    | RequestInvalidRequest String
    deriving (Show, Eq)

instance Exception RequestException

-- | Request something from the remote 'Service'.
request
    :: ( ToJSON i
       , FromJSON o
       , MonadIO m
       , MonadThrow m
       , Throws RequestException )
    => Service i o
    -> i
    -> m o
request (Service socket) request = do
    sendJson socket (Request request)
    receiveJson socket >>= \case
        Right (Response a)              -> pure a
        Right (UncaughtException error) -> throw (RequestRemoteException error)
        Right (InvalidRequest error)    -> throw (RequestInvalidRequest error)
        Right response                  -> throw (RequestUnexpectedResponse (() <$ response))
        Left error                      -> throw (RequestResponseParseError error)
