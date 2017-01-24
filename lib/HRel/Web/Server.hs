{-# LANGUAGE OverloadedStrings #-}

module HRel.Web.Server (
	webServer
) where

import           Control.Applicative
import           Control.Monad.Trans

import qualified Data.Text            as T

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           Network.HTTP.Types

import           Web.Scotty

import           HRel.Names
import           HRel.Torrents
import           HRel.Web.Templates

search :: T.Text -> Errand [Torrent]
search =
	searchForTorrents . parseTags

indexRoute :: ActionM ()
indexRoute =
	lucid (indexPage)

searchRoute :: P.Connection -> ActionM ()
searchRoute db = do
	searchTerm <- param "q"

	if T.null (T.strip searchTerm) then
		indexRoute
	else do
		result <- liftIO (runErrand db (search searchTerm))
		case result of
			Left err -> do
				liftIO (print err)
				status internalServerError500
				text "Search failed"

			Right torrents ->
				lucid (searchResultPage searchTerm torrents)

webServer :: P.Connection -> ScottyM ()
webServer db = do
	get "/" indexRoute
	get "/search" (searchRoute db <|> redirect "/")
