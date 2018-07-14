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

import Data.Binary   (Binary)
import Data.Typeable (TypeRep, Typeable, typeRep)

import HRel.Control.Exception
import HRel.Network.ZMQ

-- | Service request
data Request a
    = TypeCheck TypeRep TypeRep
    | Request a
    deriving (Show, Eq, Functor, Generic)

instance Binary a => Binary (Request a)

-- | Service response
data Response a
    = InvalidRequest String
    | UncaughtException String
    | TypeCheckFailed TypeRep TypeRep
    | TypeCheckOk
    | Response a
    deriving (Show, Eq, Functor, Generic)

instance Binary a => Binary (Response a)

-- | Serve requests from clients.
serveRequests
    :: forall i o m a
    .  ( Typeable i
       , Typeable o
       , Binary i
       , Binary o
       , MonadIO m
       , MonadCatch m )
    => Socket Rep
    -> (i -> m o)
    -> m a
serveRequests socket handleRequest =
    forever $ do
        response <- receiveBinary socket >>= \case
            Left errorMessage ->
                pure (InvalidRequest errorMessage)

            Right (TypeCheck inputType outputType)
                | inputType == realInputType && outputType == realOutputType ->
                    pure TypeCheckOk

                | otherwise ->
                    pure (TypeCheckFailed realInputType realOutputType)

            Right (Request request) ->
                catch (Response <$> handleRequest request) $ \ (SomeException e) ->
                    pure (UncaughtException (show e))

        sendBinary socket response
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
    sendBinary socket
        (TypeCheck @()
            (typeRep (id @i))
            (typeRep (id @o)))

    receiveBinary socket >>= \case
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
    :: ( Binary i
       , Binary o
       , MonadIO m
       , MonadThrow m
       , Throws RequestException )
    => Service i o
    -> i
    -> m o
request (Service socket) request = do
    sendBinary socket (Request request)
    receiveBinary socket >>= \case
        Right (Response a)              -> pure a
        Right (UncaughtException error) -> throw (RequestRemoteException error)
        Right (InvalidRequest error)    -> throw (RequestInvalidRequest error)
        Right response                  -> throw (RequestUnexpectedResponse (() <$ response))
        Left error                      -> throw (RequestResponseParseError error)
