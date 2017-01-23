{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.Trans

import           Data.Monoid

import qualified Data.Text            as T
-- import qualified Data.Text.Encoding   as T
-- import qualified Data.ByteString.Lazy as BL

-- import           Data.Aeson (object, (.=))

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           Network.URI

import           Network.HTTP.Types
-- import           Network.HTTP.Client
-- import           Network.HTTP.Client.TLS

import           Web.Scotty
import           Lucid

import           HRel.Names
import           HRel.Torrents
-- import           HRel.Feeds
-- import           HRel.Network

search :: T.Text -> Errand [Torrent]
search =
	searchForTorrents . parseTags

-- getSearch :: P.Connection -> ActionM ()
-- getSearch db = do
-- 	searchTerm <- param "q"
-- 	result <- liftIO (runErrand db (search searchTerm))

-- 	case result of
-- 		Left err -> do
-- 			liftIO (print err)
-- 			status internalServerError500
-- 			text "Search failed"

-- 		Right torrents ->
-- 			json torrents

-- postFeeds :: Manager -> P.Connection -> ActionM ()
-- postFeeds mgr db = do
-- 	bsUrl <- body
-- 	let url = either (const T.empty) id (T.decodeUtf8' (BL.toStrict bsUrl))

-- 	case parseRequest (T.unpack url) of
-- 		Just req -> do
-- 			probeResult <- liftIO (httpProbe mgr req)

-- 			if probeResult then do
-- 				result <- liftIO (runErrand db (insertFeed url))

-- 				case result of
-- 					Left err -> do
-- 						liftIO (print err)
-- 						status internalServerError500
-- 						text "Insertion failed"

-- 					Right fid ->
-- 						json (object ["id" .= fid])
-- 			else do
-- 				status badRequest400
-- 				text "Probe failed"

-- 		Nothing -> do
-- 			status badRequest400
-- 			text "Invalid URL"

-- getFeeds :: P.Connection -> ActionM ()
-- getFeeds db = do
-- 	result <- liftIO (runErrand db listFeeds)

-- 	case result of
-- 		Left err -> do
-- 			liftIO (print err)
-- 			status internalServerError500
-- 			text "Listing failed"

-- 		Right feeds ->
-- 			json (map transformResult feeds)

-- 	where
-- 		transformResult (fid, title, url) =
-- 			object ["id"    .= fid,
-- 			        "title" .= title,
-- 			        "url"   .= url]

-- getFeedContents :: P.Connection -> ActionM ()
-- getFeedContents db = do
-- 	fid <- param "id"
-- 	result <- liftIO (runErrand db (listFeedContents fid))

-- 	case result of
-- 		Left err -> do
-- 			liftIO (print err)
-- 			status internalServerError500
-- 			text "Search failed"

-- 		Right torrents ->
-- 			json (map transformResult torrents)

-- 	where
-- 		transformResult (Torrent title uri) =
-- 			object ["title" .= title,
-- 			        "uri"   .= uri]

lucid :: Html a -> ActionM ()
lucid doc = do
	setHeader "Content-Type" "text/html; charset=utf-8"
	status ok200
	raw (renderBS doc)

pageBody :: Html a -> Html ()
pageBody contents = do
	doctype_
	html_ $ do
		head_ $ do
			title_ "hrel"
			link_ [rel_ "stylesheet", type_ "text/css", href_ "/index.css"]
			meta_ [charset_ "utf-8"]

		body_ $ do
			div_ [id_ "canvas"] contents
			div_ [id_ "footer"] $
				a_ [class_ "link", target_ "blank", href_ "https://github.com/vapourismo/hrel"]
					"hrel"

searchForm :: Maybe T.Text -> Html ()
searchForm mbSearchTerm =
	form_ [class_ "search-form", action_ "/search"] $ do
		case mbSearchTerm of
			Just searchTerm -> input_ [class_ "input", name_ "q", type_ "text", autofocus_,
			                           value_ searchTerm]
			Nothing         -> input_ [class_ "input", name_ "q", type_ "text", autofocus_]

		div_ [class_ "submit-button"] "Search"

searchResult :: Torrent -> Html ()
searchResult (Torrent title url) =
	div_ [class_ "result"] $ do
		div_ [class_ "cell title"] (toHtml title)
		a_ [class_ "cell link", target_ "blank", href_ url] "link"
		a_ [class_ "cell add", target_ "blank", href_ addUrl] "add"
	where
		addUrl =
			"https://www.premiumize.me/downloader?magnet="
			<> T.concatMap (T.pack . escapeURIChar isUnescapedInURIComponent) url

indexPage :: ActionM ()
indexPage =
	lucid $ pageBody $
		div_ [class_ "content"] $
			searchForm Nothing

searchPage :: P.Connection -> ActionM ()
searchPage db = do
	searchTerm <- param "q"
	result <- liftIO (runErrand db (search searchTerm))

	case result of
		Left err -> do
			liftIO (print err)
			status internalServerError500
			text "Search failed"

		Right torrents ->
			lucid $ pageBody $
				div_ [class_ "content"] $ do
					searchForm (Just searchTerm)
					div_ [class_ "results"] $
						mapM_ searchResult torrents

main :: IO ()
main = do
	db <- P.connectdb "user=hrel dbname=hrel"
	-- mgr <- newManager tlsManagerSettings

	scotty 3401 $ do
		get "/" indexPage
		get "/search" (searchPage db <|> redirect "/")
		-- get "/search" (getSearch db)
		-- get "/feeds" (getFeeds db)
		-- post "/feeds" (postFeeds mgr db)
		-- get "/feeds/:id" (getFeedContents db)
