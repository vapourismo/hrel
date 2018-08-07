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

    , SocketRecipe (..)
    , ZMQ.Socket
    , withSocket
    , makeSocket
    , closeSocket

    , send
    , send'
    , sendBinary

    , receive
    , receiveBinary

    , readConnectM
    , readBindM
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

-- | Do something with a 'ZMQ.Context'. The context will terminate after the handler returns.
withContext
    :: ( MonadIO m
       , MonadMask m
       , Throws ZMQ.ZMQError
       )
    => (ZMQ.Context -> m a)
    -> m a
withContext =
    bracket (liftIO ZMQ.context) (liftIO . ZMQ.term)

-- | ZMQ monad
newtype ZMQ a =
    ZMQ (ReaderT ZMQ.Context IO a)
    deriving (Functor, Applicative, Monad)

-- | Execute the ZMQ operations within a given 'ZMQ.Context'.
runZMQ :: MonadIO m => ZMQ.Context -> ZMQ a -> m a
runZMQ context (ZMQ (ReaderT reader)) =
    liftIO (reader context)

-- | Monad that supports ZMQ operations
class MonadZMQ m where
    liftZMQ :: ZMQ a -> m a

instance MonadZMQ ZMQ where
    liftZMQ = id

instance MonadIO m => MonadZMQ (ReaderT ZMQ.Context m) where
    liftZMQ (ZMQ reader) = mapReaderT liftIO reader

instance (Monad m, MonadZMQ m) => MonadZMQ (ResourceT m) where
    liftZMQ = lift . liftZMQ

-- | Socket recipe
data SocketRecipe a
    = Bind a String
    | Connect a String
    deriving (Show, Eq)

-- | Do something with a 'ZMQ.Socket'. The socket will close after the handler returns.
withSocket
    :: ( ZMQ.SocketType t
       , MonadMask m
       , MonadZMQ m
       , Throws ZMQ.ZMQError
       )
    => SocketRecipe t
    -> (ZMQ.Socket t -> m a)
    -> m a
withSocket recipe =
    bracket (makeSocket recipe) closeSocket

-- | Create a new 'ZMQ.Socket'.
makeSocket
    :: ( ZMQ.SocketType t
       , MonadZMQ m
       , Throws ZMQ.ZMQError
       )
    => SocketRecipe t
    -> m (ZMQ.Socket t)
makeSocket recipe =
    liftZMQ $ ZMQ $ ReaderT $ \ context ->
        case recipe of
            Bind typ info -> do
                socket <- ZMQ.socket context typ
                socket <$ ZMQ.bind socket info

            Connect typ info -> do
                socket <- ZMQ.socket context typ
                socket <$ ZMQ.connect socket info

-- | Close a 'ZMQ.Socket'.
closeSocket :: (MonadZMQ m, Throws ZMQ.ZMQError) => ZMQ.Socket t -> m ()
closeSocket =
    liftZMQ . ZMQ . lift . ZMQ.close

-- | Send a message through the 'ZMQ.Socket'.
send
    :: ( ZMQ.Sender t
       , MonadZMQ m
       , Throws ZMQ.ZMQError
       )
    => ZMQ.Socket t
    -> ByteString
    -> m ()
send sock message =
    liftZMQ (ZMQ (lift (ZMQ.send sock [] message)))

-- | Send a message through the 'ZMQ.Socket'.
send'
    :: ( ZMQ.Sender t
       , MonadZMQ m
       , Throws ZMQ.ZMQError
       )
    => ZMQ.Socket t
    -> LazyByteString.ByteString
    -> m ()
send' sock message =
    liftZMQ (ZMQ (lift (ZMQ.send' sock [] message)))

-- | Receive a message through the 'ZMQ.Socket'.
receive
    :: ( ZMQ.Receiver t
       , MonadZMQ m
       , Throws ZMQ.ZMQError
       )
    => ZMQ.Socket t
    -> m ByteString
receive =
    liftZMQ . ZMQ . lift . ZMQ.receive

-- | Send a 'Binary'-encoded value through the 'ZMQ.Socket'.
sendBinary
    :: ( ZMQ.Sender t
       , Binary.Binary a
       , MonadZMQ m
       , Throws ZMQ.ZMQError
       )
    => ZMQ.Socket t
    -> a
    -> m ()
sendBinary socket message =
    send' socket (Binary.encode message)

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
       , Throws ZMQ.ZMQError
       , Throws DecodeException
       )
    => ZMQ.Socket t
    -> m a
receiveBinary socket = do
    message <- receive socket
    case Binary.decodeOrFail (LazyByteString.fromStrict message) of
        Left (_, offset, errorMessage) -> throw (DecodeException message offset errorMessage)
        Right (_, _, result)           -> pure result

readConnectM :: ZMQ.SocketType a => a -> ReadM (SocketRecipe a)
readConnectM typ = Connect typ <$> str

readBindM :: ZMQ.SocketType a => a -> ReadM (SocketRecipe a)
readBindM typ = Bind typ <$> str
