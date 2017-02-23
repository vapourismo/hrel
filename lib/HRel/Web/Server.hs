{-# LANGUAGE OverloadedStrings #-}

module HRel.Web.Server (
	webServer
) where

import           Control.Applicative
import           Control.Monad.Trans

import           Data.Int
import qualified Data.Text            as T

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           Network.HTTP.Types

import           Web.Scotty

import           HRel.Names
import           HRel.Torrents
import           HRel.Feeds
import           HRel.Web.Templates

search :: T.Text -> Errand [Torrent]
search =
	searchForTorrents . parseTags

indexRoute :: P.Connection -> ActionM ()
indexRoute db = do
	result <- liftIO (runErrand db listFeeds)

	case result of
		Left err -> do
			liftIO (print err)
			status internalServerError500
			text "Something went wrong"

		Right feeds ->
			lucid (feedsOverviewPage feeds)

searchRoute :: P.Connection -> ActionM ()
searchRoute db = do
	searchTerm <- param "q"

	if T.null (T.strip searchTerm) then
		indexRoute db
	else do
		result <- liftIO (runErrand db (search searchTerm))
		case result of
			Left err -> do
				liftIO (print err)
				status internalServerError500
				text "Search failed"

			Right torrents ->
				lucid (searchResultPage searchTerm torrents)

feedContentsRoute :: P.Connection -> ActionM ()
feedContentsRoute db = do
	fid <- param "fid" :: ActionM Int64
	result <- liftIO (runErrand db (listFeedContents fid))

	case result of
		Left err -> do
			liftIO (print err)
			status internalServerError500
			text "Something went wrong"

		Right torrents ->
			lucid (feedContentsPage torrents)

webServer :: P.Connection -> ScottyM ()
webServer db = do
	get "/" (indexRoute db)
	get "/search" (searchRoute db <|> redirect "/")
	get "/feeds/:fid" (feedContentsRoute db)
