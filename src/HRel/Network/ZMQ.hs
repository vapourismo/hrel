{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HRel.Network.ZMQ
    ( ZMQ.Pull (..)
    , ZMQ.Push (..)
    , ZMQ.Req (..)
    , ZMQ.Rep (..)

    , ZMQ.ZMQError
    , DecodeException (..)

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

    , receive
    , receiveBinary

    , connectedSocketReadM
    , boundSocketReadM
    )
where

import GHC.Generics (Generic)

import Control.Monad.Catch          (MonadMask, bracket)
import Control.Monad.Reader         (ReaderT (..), mapReaderT)
import Control.Monad.Trans          (MonadIO (liftIO), lift)
import Control.Monad.Trans.Resource (ResourceT)

import Options.Applicative

import qualified Data.Binary          as Binary
import           Data.Binary.Get      (ByteOffset)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString

import qualified System.ZMQ4 as ZMQ

import HRel.Control.Exception

withContext :: (MonadIO m, MonadMask m) => (ZMQ.Context -> m a) -> m a
withContext = bracket (liftIO ZMQ.context) (liftIO . ZMQ.term)

newtype ZMQ a = ZMQ (ReaderT ZMQ.Context IO a)
    deriving (Functor, Applicative, Monad)

runZMQ :: MonadIO m => ZMQ.Context -> ZMQ a -> m a
runZMQ context (ZMQ (ReaderT reader)) = liftIO (reader context)

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
sendBinary :: (ZMQ.Sender t, Binary.Binary a, MonadZMQ m) => ZMQ.Socket t -> a -> m ()
sendBinary socket message =
    send socket (LazyByteString.toStrict (Binary.encode message))

data DecodeException =
    DecodeException
        { decodeExceptionInput   :: ByteString
        , decodeExceptionOffset  :: ByteOffset
        , decodeExceptionMessage :: String
        }
    deriving (Show, Eq, Generic)

instance Binary.Binary DecodeException

instance Exception DecodeException

-- | Receive a 'Binary'-encoded value from the 'ZMQ.Socket'.
receiveBinary
    :: ( ZMQ.Receiver t
       , Binary.Binary a
       , MonadThrow m
       , MonadZMQ m
       , Throws DecodeException )
    => ZMQ.Socket t
    -> m a
receiveBinary socket = do
    message <- receive socket
    case Binary.decodeOrFail (LazyByteString.fromStrict message) of
        Left (_, offset, errorMessage) -> throw (DecodeException message offset errorMessage)
        Right (_, _, result)           -> pure result

connectedSocketReadM
    :: (ZMQ.SocketType a, MonadZMQ m)
    => a
    -> ReadM (SmallBomb ZMQ.ZMQError m (ZMQ.Socket a))
connectedSocketReadM typ =
    flip fmap str $ \ info -> Bomb $ liftZMQ $ do
        socket <- makeSocket typ
        socket <$ connect socket info

boundSocketReadM
    :: (ZMQ.SocketType a, MonadZMQ m)
    => a
    -> ReadM (SmallBomb ZMQ.ZMQError m (ZMQ.Socket a))
boundSocketReadM typ =
    flip fmap str $ \ info -> Bomb $ liftZMQ $ do
        socket <- makeSocket typ
        socket <$ bind socket info
