{-# LANGUAGE RecordWildCards #-}

module HRel.Application.FeedProcessor
    ( Input
    , inputInfo
    , main )
where

import Control.Monad                (forever)
import Control.Monad.Trans          (MonadIO (liftIO))
import Control.Monad.Trans.Resource

import Options.Applicative

import HRel.Network.Types
import HRel.Network.ZMQ   as ZMQ

newtype Input =
    Input
        { inputWithPullSocket :: ZMQ.Context -> ResourceT IO (ZMQ.Socket ZMQ.Pull) }

inputInfo :: ParserInfo Input
inputInfo =
    info inputP (progDesc "Feed processor")
    where
        inputP =
            Input <$> argument (connectedSocketReadM ZMQ.Pull) (metavar "CONNECTINFO")

main :: Input -> IO ()
main Input{..} = runResourceT $ do
    context <- ZMQ.makeContext
    socket <- inputWithPullSocket context

    forever $ do
        message <- ZMQ.receiveJson socket
        liftIO (print (message :: Either String FeedProcessRequest))
