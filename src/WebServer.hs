{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Network.Wai.Handler.Warp

import           Web.Scotty (scottyApp)

import           System.Posix.Env.ByteString

import           HRel.Database
import           HRel.Web.Server

-- | Web server settings
webSettings :: Settings
webSettings =
	setPort 3401 $
	setServerName "hrel"
		defaultSettings

-- | Run the web server.
main :: IO ()
main = do
	connStr <- getEnvDefault "PGINFO" "host=localhost user=hrel dbname=hrel"
	db <- connectDatabase connStr
	app <- scottyApp (webServer db)

	runSettings webSettings app
