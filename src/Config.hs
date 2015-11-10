{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config (
	confListenPort,
	confHourlyDump
) where

import           Control.Exception
import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL

import           System.Environment
import           System.IO.Unsafe

data Config = Config {
	_confListenPort :: Int,
	_confHourlyDump :: Maybe String
}

instance FromJSON Config where
	parseJSON (Object val) =
		Config <$> val .: "listen_port"
		       <*> val .:? "hourly_dump"
	parseJSON _ = mzero

confListenPort :: Int
confHourlyDump :: Maybe String

Config confListenPort
       confHourlyDump =
	maybe defaultConfig id (unsafePerformIO (readContents `catch` \ (SomeException _) -> pure Nothing))
	where
		readContents = do
			home <- getEnv "HOME"
			decode <$> BL.readFile (home ++ "/.hrel")

		defaultConfig =
			Config 3000 Nothing
