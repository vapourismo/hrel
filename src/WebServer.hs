{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Network.Wai.Handler.Warp

import           Web.Scotty (scottyApp)

import           HRel.Database
import           HRel.Web.Server

-- | Web server settings
settings :: Settings
settings =
	setPort 3401
	$ setServerName "hrel"
	$ defaultSettings

-- | Run the web server.
main :: IO ()
main = do
	db <- connectDatabase "user=hrel dbname=hrel"
	app <- scottyApp (webServer db)

	runSettings settings app
