{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module HRel.Application.Hub
    ( Request (..)
    , Response (..)

      -- * Application
    , Input
    , inputInfo
    , main )
where

import GHC.Generics (Generic)

import Control.Monad.Trans          (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Data.Aeson    (FromJSON, ToJSON)
import Data.Binary   (Binary)
import Data.Monoid   (mconcat)
import Data.Typeable (Typeable)

import Options.Applicative

import           HRel.Network.Service
import qualified HRel.Network.ZMQ     as ZMQ

newtype Request
    = DistributeFeed
        { url :: String }
    deriving (Show, Typeable, Generic)

instance Binary Request

instance FromJSON Request

instance ToJSON Request

data Response
    = Ok
    deriving (Show, Typeable, Generic)

instance Binary Response

instance FromJSON Response

instance ToJSON Response

newtype Input =
    Input
        { inputMakeCommandSocket :: ZMQ.Context -> ResourceT IO (ZMQ.Socket ZMQ.Rep) }

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
main Input{..} = runResourceT $ do
    context <- ZMQ.makeContext
    commandSocket <- inputMakeCommandSocket context

    serveRequests commandSocket $ \case
        DistributeFeed url -> do
            liftIO (print url)
            pure Ok
