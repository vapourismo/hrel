{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HRel.Network.ZMQ
    ( ZMQ.Pull (..)
    , ZMQ.Push (..)
    , ZMQ.Req (..)
    , ZMQ.Rep (..)

    , ZMQ.ZMQError

    , ZMQ
    , runZMQ
    , MonadZMQ (..)

    , ZMQ.Context
    , withContext

    , ZMQ.Socket
    , makeSocket
    , connect
    , bind

    , send
    , sendBinary
    , sendJson

    , receive
    , receiveBinary
    , receiveJson

    , connectedSocketReadM
    , boundSocketReadM
    )
where

import Control.Monad.Catch          (MonadMask, bracket)
import Control.Monad.Reader         (ReaderT (..), mapReaderT)
import Control.Monad.Trans          (MonadIO (liftIO), lift)
import Control.Monad.Trans.Resource (ResourceT)

import Options.Applicative

import qualified Data.Aeson           as Aeson
import qualified Data.Binary          as Binary
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString

import qualified System.ZMQ4 as ZMQ

withContext :: (MonadIO m, MonadMask m) => (ZMQ.Context -> m a) -> m a
withContext = bracket (liftIO ZMQ.context) (liftIO . ZMQ.term)

newtype ZMQ a = ZMQ (ReaderT ZMQ.Context IO a)
    deriving (Functor, Applicative, Monad)

runZMQ :: ZMQ.Context -> ZMQ a -> IO a
runZMQ context (ZMQ (ReaderT reader)) = reader context

class MonadZMQ m where
    liftZMQ :: ZMQ a -> m a

instance MonadZMQ ZMQ where
    liftZMQ = id

instance MonadIO m => MonadZMQ (ReaderT ZMQ.Context m) where
    liftZMQ (ZMQ reader) = mapReaderT liftIO reader

instance (Monad m, MonadZMQ m) => MonadZMQ (ResourceT m) where
    liftZMQ = lift . liftZMQ

makeSocket :: (ZMQ.SocketType t, MonadZMQ m) => t -> m (ZMQ.Socket t)
makeSocket typ = liftZMQ (ZMQ (ReaderT (`ZMQ.socket` typ)))

connect :: (ZMQ.SocketType t, MonadZMQ m) => ZMQ.Socket t -> String -> m ()
connect sock info = liftZMQ (ZMQ (lift (ZMQ.connect sock info)))

bind :: (ZMQ.SocketType t, MonadZMQ m) => ZMQ.Socket t -> String -> m ()
bind sock info = liftZMQ (ZMQ (lift (ZMQ.bind sock info)))

send :: (ZMQ.Sender t, MonadZMQ m) => ZMQ.Socket t -> ByteString -> m ()
send sock message = liftZMQ (ZMQ (lift (ZMQ.send sock [] message)))

receive :: (ZMQ.Receiver t, MonadZMQ m) => ZMQ.Socket t -> m ByteString
receive sock = liftZMQ (ZMQ (lift (ZMQ.receive sock)))

-- | Send a 'Binary'-encoded value through the 'ZMQ.Socket'.
sendBinary
    :: (ZMQ.Sender t, Binary.Binary a, MonadIO m) => ZMQ.Socket t -> a -> m ()
sendBinary socket message =
    liftIO (ZMQ.send socket [] (LazyByteString.toStrict (Binary.encode message)))

-- | Receive a 'Binary'-encoded value from the 'ZMQ.Socket'.
receiveBinary
    :: (ZMQ.Receiver t, Binary.Binary a, MonadIO m)
    => ZMQ.Socket t
    -> m (Either String a)
receiveBinary socket = do
    message <- liftIO (ZMQ.receive socket)
    pure $ case Binary.decodeOrFail (LazyByteString.fromStrict message) of
        Left (_, _, errorMessage) -> Left errorMessage
        Right (_, _, result)      -> Right result

-- | Send a JSON-encoded value.
sendJson :: (ZMQ.Sender t, Aeson.ToJSON a, MonadIO m) => ZMQ.Socket t -> a -> m ()
sendJson socket message =
    liftIO (ZMQ.send socket [] (LazyByteString.toStrict (Aeson.encode message)))

-- | Receive a JSON-encoded value.
receiveJson :: (ZMQ.Receiver t, Aeson.FromJSON a, MonadIO m) => ZMQ.Socket t -> m (Either String a)
receiveJson socket =
    Aeson.eitherDecode . LazyByteString.fromStrict <$> liftIO (ZMQ.receive socket)

connectedSocketReadM
    :: ( ZMQ.SocketType a
       , MonadZMQ m
       )
    => a
    -> ReadM (m (ZMQ.Socket a))
connectedSocketReadM typ =
    flip fmap str $ \ info -> liftZMQ $ do
        socket <- makeSocket typ
        socket <$ connect socket info

boundSocketReadM
    :: ( ZMQ.SocketType a
       , MonadZMQ m
       )
    => a
    -> ReadM (m (ZMQ.Socket a))
boundSocketReadM typ =
    flip fmap str $ \ info -> liftZMQ $ do
        socket <- makeSocket typ
        socket <$ bind socket info
