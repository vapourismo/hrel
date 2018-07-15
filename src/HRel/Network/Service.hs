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
    , request
    )
where

import GHC.Generics (Generic)

import Control.Exception (SomeException (..))
import Control.Monad     (forever)

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
    = InvalidRequest DecodeException  -- ^ Incoming request could not be parsed
    | UncaughtException String        -- ^ An uncaught exception propagated from the handler
    | TypeCheckFailed TypeRep TypeRep -- ^ Type check failed
    | TypeCheckOk                     -- ^ Type check succeeded
    | Response a                      -- ^ Plain response
    deriving (Show, Eq, Functor, Generic)

instance Binary a => Binary (Response a)

-- | Serve requests from clients.
serveRequests
    :: forall i o m a
    .  ( Typeable i
       , Typeable o
       , Binary i
       , Binary o
       , MonadCatch m
       , MonadMask m
       , MonadZMQ m
       , Throws ZMQError
       )
    => SocketRecipe Rep
    -> (i -> m o)
    -> m a
serveRequests recipe handleRequest =
    withSocket recipe $ \ socket -> forever $ do
        response <- try @DecodeException (receiveBinary socket) >>= \case
            Left decodeError ->
                pure (InvalidRequest decodeError)

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
    = IntroDecodeException DecodeException
    | IntroUnexpectedResponse (Response ())
    | IntroZmqError ZMQError
    deriving (Show, Eq)

instance Exception IntroException

-- | Introduce to the remote 'Service'.
introduce
    :: forall i o m
    .  ( Typeable i
       , Typeable o
       , MonadCatch m
       , MonadZMQ m
       , Throws IntroException
       )
    => Socket Req
    -> m (Service i o)
introduce socket = mapException IntroZmqError $ do
    sendBinary socket $
        TypeCheck @()
            (typeRep (id @i))
            (typeRep (id @o))

    mapException IntroDecodeException $
        receiveBinary socket >>= \case
            TypeCheckOk -> pure (Service socket)
            response    -> throw (IntroUnexpectedResponse response)

-- | Exception that may occur during request
data RequestException
    = RequestDecodeException DecodeException
    | RequestUnexpectedResponse (Response ())
    | RequestRemoteException String
    | RequestInvalid DecodeException
    | RequestZmqError ZMQError
    deriving (Show, Eq)

instance Exception RequestException

-- | Request something from the remote 'Service'.
request
    :: ( Binary i
       , Binary o
       , MonadCatch m
       , MonadZMQ m
       , Throws RequestException
       )
    => Service i o
    -> i
    -> m o
request (Service socket) request = mapException RequestZmqError $ do
    sendBinary socket (Request request)
    mapException RequestDecodeException $
        receiveBinary socket >>= \case
            Response a              -> pure a
            UncaughtException error -> throw (RequestRemoteException error)
            InvalidRequest error    -> throw (RequestInvalid error)
            response                -> throw (RequestUnexpectedResponse (() <$ response))
