{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module HRel.Application.Hub
    ( Request (..)
    , Response (..)

      -- * Application
    , Input
    , inputInfo
    , main )
where

import GHC.Generics (Generic)

import Control.Monad.Reader         (runReaderT)
import Control.Monad.Trans          (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

import Data.Binary   (Binary)
import Data.Monoid   (mconcat)
import Data.Typeable (Typeable)

import Options.Applicative

import           HRel.Control.Exception
import           HRel.Network.Service
import qualified HRel.Network.ZMQ       as ZMQ

newtype Request
    = DistributeFeed
        { url :: String }
    deriving (Show, Typeable, Generic)

instance Binary Request

data Response
    = Ok
    deriving (Show, Typeable, Generic)

instance Binary Response

newtype Input =
    Input
        { inputMakeCommandSocket :: SmallBomb ZMQ.ZMQError ZMQ.ZMQ (ZMQ.Socket ZMQ.Rep) }

inputInfo :: ParserInfo Input
inputInfo =
    info inputP (progDesc "Hub")
    where
        inputP = Input <$> commandSocketP

        commandSocketP =
            option (ZMQ.boundSocketReadM ZMQ.Rep) $ mconcat
                [ long "command-socket"
                , metavar "BINDINFO" ]

main :: Input -> IO ()
main Input{..} =
    failOnException @ZMQ.ZMQError $ ZMQ.withContext $ \ context ->
        runResourceT $ flip runReaderT context $ do
            commandSocket <- ZMQ.liftZMQ (defuse inputMakeCommandSocket)

            serveRequests commandSocket $ \case
                DistributeFeed url -> do
                    liftIO (print url)
                    pure Ok
