{-# LANGUAGE RecordWildCards #-}

module HRel.Application.Hub
    ( Input
    , inputInfo
    , main )
where

import Control.Monad.Trans          (MonadIO (liftIO))
import Control.Monad.Trans.Resource

import Control.Concurrent
import Control.Monad

import Options.Applicative

import HRel.Network.Types
import HRel.Network.ZMQ   as ZMQ

newtype Input =
    Input
        { inputWithPushSocket :: ZMQ.Context -> ResourceT IO (ZMQ.Socket ZMQ.Push) }

inputInfo :: ParserInfo Input
inputInfo =
    info inputP (progDesc "Hub")
    where
        inputP = Input <$> argument (boundSocketReadM ZMQ.Push) (metavar "BINDINFO")

main :: Input -> IO ()
main Input{..} = runResourceT $ do
    context <- ZMQ.makeContext
    socket <-  inputWithPushSocket context

    forever $ do
        liftIO (threadDelay 1000000)
        sendJson socket (FeedProcessRequest "Hello World")

