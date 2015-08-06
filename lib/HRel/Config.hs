{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HRel.Config (
	confListenPort
) where

import           Control.Monad
import           Control.Exception

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL

import           System.IO.Unsafe
import           System.Environment

data Config = Config {
	_confListenPort :: Int
}

instance FromJSON Config where
	parseJSON (Object val) =
		Config <$> val .: "listen_port"
	parseJSON _ = mzero

confListenPort :: Int

Config confListenPort =
	maybe defaultConfig id (unsafePerformIO (readContents `catch` \ (SomeException _) -> pure Nothing))
	where
		readContents = do
			home <- getEnv "HOME"
			decode <$> BL.readFile (home ++ "/.hrel")

		defaultConfig =
			Config 3000
