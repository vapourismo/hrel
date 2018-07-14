{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module HRel.Application.FeedProcessor
    ( Input
    , inputInfo
    , main )
where

import Control.Monad.Trans          (MonadIO (liftIO))
import Control.Monad.Trans.Resource

import Options.Applicative

import           HRel.Application.Hub   (Request (..), Response (..))
import           HRel.Control.Exception
import           HRel.Network.Service
import qualified HRel.Network.ZMQ       as ZMQ

newtype Input =
    Input
        { inputWithReqSocket :: ZMQ.Context -> ResourceT IO (ZMQ.Socket ZMQ.Req) }

inputInfo :: ParserInfo Input
inputInfo =
    info inputP (progDesc "Feed processor")
    where
        inputP = Input <$> argument (ZMQ.connectedSocketReadM ZMQ.Req) (metavar "CONNECTINFO")

main :: Input -> IO ()
main Input{..} = runResourceT $ do
    context <- ZMQ.makeContext
    socket  <- inputWithReqSocket context

    Right service <- try @IntroException (introduce @Request @Response socket)

    res <- try @RequestException (request service (DistributeFeed "http://example.com/feed.xml"))
    liftIO (print res)
