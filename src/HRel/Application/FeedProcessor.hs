{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module HRel.Application.FeedProcessor
    ( Input
    , inputInfo
    , main )
where

import Control.Monad.Reader         (runReaderT)
import Control.Monad.Trans          (MonadIO (liftIO))
import Control.Monad.Trans.Resource (runResourceT)

import Options.Applicative

import           HRel.Application.Hub   (Request (..), Response (..))
import           HRel.Control.Exception
import           HRel.Network.Service
import qualified HRel.Network.ZMQ       as ZMQ

newtype Input =
    Input
        { inputWithReqSocket :: SmallBomb ZMQ.ZMQError ZMQ.ZMQ (ZMQ.Socket ZMQ.Req) }

inputInfo :: ParserInfo Input
inputInfo =
    info inputP (progDesc "Feed processor")
    where
        inputP = Input <$> argument (ZMQ.connectedSocketReadM ZMQ.Req) (metavar "CONNECTINFO")

main :: Input -> IO ()
main Input{..} =
    failOnException @ZMQ.ZMQError $ ZMQ.withContext $ \ context ->
        runResourceT $ flip runReaderT context $ do
            socket <- ZMQ.liftZMQ (defuse inputWithReqSocket)

            Right service <- try @IntroException (introduce @Request @Response socket)

            res <-
                try @RequestException
                    (request service (DistributeFeed "http://example.com/feed.xml"))
            liftIO (print res)
