{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans

import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.ByteString.Lazy as BL

import           Data.Aeson (object, (.=))

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           Network.HTTP.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Web.Scotty

import           HRel.Names
import           HRel.Torrents
import           HRel.Feeds
import           HRel.Network

search :: T.Text -> Errand [Torrent]
search =
	searchForTorrents . parseTags

getSearch :: P.Connection -> ActionM ()
getSearch db = do
	searchTerm <- param "q"
	result <- liftIO (runErrand db (search searchTerm))

	case result of
		Left err -> do
			liftIO (print err)
			status internalServerError500
			text "Search failed"

		Right torrents ->
			json torrents

postFeeds :: Manager -> P.Connection -> ActionM ()
postFeeds mgr db = do
	bsUrl <- body
	let url = either (const T.empty) id (T.decodeUtf8' (BL.toStrict bsUrl))

	case parseRequest (T.unpack url) of
		Just req -> do
			probeResult <- liftIO (httpProbe mgr req)

			if probeResult then do
				result <- liftIO (runErrand db (insertFeed url))

				case result of
					Left err -> do
						liftIO (print err)
						status internalServerError500
						text "Insertion failed"

					Right fid ->
						json (object ["id" .= fid])
			else do
				status badRequest400
				text "Probe failed"

		Nothing -> do
			status badRequest400
			text "Invalid URL"

getFeeds :: P.Connection -> ActionM ()
getFeeds db = do
	result <- liftIO (runErrand db listFeeds)

	case result of
		Left err -> do
			liftIO (print err)
			status internalServerError500
			text "Listing failed"

		Right feeds ->
			json (map transformResult feeds)

	where
		transformResult (fid, title, url) =
			object ["id"    .= fid,
			        "title" .= title,
			        "url"   .= url]

getFeedContents :: P.Connection -> ActionM ()
getFeedContents db = do
	fid <- param "id"
	result <- liftIO (runErrand db (listFeedContents fid))

	case result of
		Left err -> do
			liftIO (print err)
			status internalServerError500
			text "Search failed"

		Right torrents ->
			json (map transformResult torrents)

	where
		transformResult (Torrent title uri) =
			object ["title" .= title,
			        "uri"   .= uri]

main :: IO ()
main = do
	db <- P.connectdb "user=hrel dbname=hrel"
	mgr <- newManager tlsManagerSettings

	scotty 3401 $ do
		get "/search" (getSearch db)
		get "/feeds" (getFeeds db)
		post "/feeds" (postFeeds mgr db)
		get "/feeds/:id" (getFeedContents db)
