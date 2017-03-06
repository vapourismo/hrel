{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module HRel.Web.Server (
	webServer
) where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource

import           Data.Int
import qualified Data.Text         as T
import           Data.Conduit
import qualified Data.Conduit.List as C

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

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

addFeedRoute :: ActionM ()
addFeedRoute =
	lucid addFeedPage

addFeedPostRoute :: P.Connection -> ActionM ()
addFeedPostRoute db = do
	mgr <- liftIO (newManager tlsManagerSettings)
	url <- param "url" :: ActionM String

	res <- fetchFeedContents mgr url

	case res of
		Left err -> do
			liftIO (print err)
			status internalServerError500
			text "Something went wrong"

		_ -> redirect "/"

	where
		fetchFeedContents mgr url =
			liftIO (runResourceT (runExceptT (feedSource @(ResourceT IO) mgr url
			                                  $$ processFeedContents url)))

		processFeedContents url =
			C.mapM_ $ \ (Feed title cnt) -> liftIO $ do
				print cnt
				result <- runErrand db $ do
					fid <- insertFeed url
					optional (updateFeedTitle fid title)

				case result of
					Left err -> print err
					Right _  -> pure ()

webServer :: P.Connection -> ScottyM ()
webServer db = do
	get "/" (indexRoute db)
	get "/search" (searchRoute db <|> redirect "/")
	get "/feeds/new" addFeedRoute
	post "/feeds/new" (addFeedPostRoute db)
	get "/feeds/:fid" (feedContentsRoute db)
