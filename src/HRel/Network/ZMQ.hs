module HRel.Network.ZMQ
    ( ZMQ.Pull (..)
    , ZMQ.Push (..)
    , ZMQ.Req (..)
    , ZMQ.Rep (..)

    , ZMQ.Context
    , makeContext

    , ZMQ.Socket
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

import Control.Monad.Trans          (MonadIO (liftIO))
import Control.Monad.Trans.Resource

import Options.Applicative

import qualified Data.Aeson           as Aeson
import qualified Data.Binary          as Binary
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString

import qualified System.ZMQ4 as ZMQ

-- | Create a new 'ZMQ.Context'.
makeContext :: MonadResource m => m ZMQ.Context
makeContext = snd <$> allocate ZMQ.context ZMQ.term

-- | Connect to a remote 'ZMQ.Socket'.
connect :: (ZMQ.SocketType a, MonadResource m) => ZMQ.Context -> a -> String -> m (ZMQ.Socket a)
connect context typ info = do
    (_, socket) <- allocate (ZMQ.socket context typ) ZMQ.close
    socket <$ liftIO (ZMQ.connect socket info)

-- | Bind a local 'ZMQ.Socket'.
bind :: (ZMQ.SocketType a, MonadResource m) => ZMQ.Context -> a -> String -> m (ZMQ.Socket a)
bind context typ info = do
    (_, socket) <- allocate (ZMQ.socket context typ) ZMQ.close
    socket <$ liftIO (ZMQ.bind socket info)

-- | Send a message through the 'ZMQ.Socket'.
send :: (ZMQ.Sender t, MonadIO m) => ZMQ.Socket t -> ByteString -> m ()
send socket message = liftIO (ZMQ.send socket [] message)

-- | Send a 'Binary'-encoded value through the 'ZMQ.Socket'.
sendBinary :: (ZMQ.Sender t, Binary.Binary a, MonadIO m) => ZMQ.Socket t -> a -> m ()
sendBinary socket message =
    liftIO (ZMQ.send socket [] (LazyByteString.toStrict (Binary.encode message)))

-- | Send a JSON-encoded value.
sendJson :: (ZMQ.Sender t, Aeson.ToJSON a, MonadIO m) => ZMQ.Socket t -> a -> m ()
sendJson socket message =
    liftIO (ZMQ.send socket [] (LazyByteString.toStrict (Aeson.encode message)))

-- | Receive a message from the 'ZMQ.Socket'.
receive :: (ZMQ.Receiver t, MonadIO m) => ZMQ.Socket t -> m ByteString
receive socket = liftIO (ZMQ.receive socket)

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

-- | Receive a JSON-encoded value.
receiveJson :: (ZMQ.Receiver t, Aeson.FromJSON a, MonadIO m) => ZMQ.Socket t -> m (Either String a)
receiveJson socket =
    Aeson.eitherDecode . LazyByteString.fromStrict <$> liftIO (ZMQ.receive socket)

-- |
connectedSocketReadM
    :: (ZMQ.SocketType a, MonadResource m)
    => a
    -> ReadM (ZMQ.Context -> m (ZMQ.Socket a))
connectedSocketReadM typ =
    (\ info context -> connect context typ info) <$> str

-- |
boundSocketReadM
    :: (ZMQ.SocketType a, MonadResource m)
    => a
    -> ReadM (ZMQ.Context -> m (ZMQ.Socket a))
boundSocketReadM typ =
    (\ info context -> bind context typ info) <$> str
