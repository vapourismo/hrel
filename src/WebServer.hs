{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Network.Wai.Handler.Warp

import           Web.Scotty (scottyApp)

import qualified Data.ByteString as B

import           HRel.Database
import           HRel.Web.Server

-- | Web server settings
webSettings :: Settings
webSettings =
	setPort 3401
	$ setServerName "hrel"
	$ defaultSettings

-- | PostgreSQL connection string
dbConnString :: B.ByteString
dbConnString = "host=localhost user=hrel dbname=hrel"

-- | Run the web server.
main :: IO ()
main = do
	db <- connectDatabase dbConnString
	app <- scottyApp (webServer db)

	runSettings webSettings app
