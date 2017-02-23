{-# LANGUAGE OverloadedStrings #-}

module HRel.Web.Templates (
	lucid,
	indexPage,
	searchResultPage,
	feedsOverviewPage,
	feedContentsPage
) where

import           Control.Monad

import           Data.Int
import           Data.Monoid
import qualified Data.Text as T

import           Lucid
import           Network.URI

import           Web.Scotty
import           Network.HTTP.Types

import           HRel.Torrents

-- | Render template and return with status 200.
lucid :: Html a -> ActionM ()
lucid doc = do
	setHeader "Content-Type" "text/html; charset=utf-8"
	status ok200
	raw (renderBS doc)

-- | Outer document body
documentBody :: Html a -> Html ()
documentBody contents = do
	doctype_
	html_ $ do
		head_ $ do
			title_ "hrel"
			link_ [rel_ "stylesheet", type_ "text/css", href_ "/index.css"]
			meta_ [charset_ "utf-8"]

		body_ $ do
			div_ [id_ "canvas"] $
				div_ [class_ "content"]
					contents

			div_ [id_ "footer"] $
				a_ [class_ "link", target_ "blank", href_ "https://github.com/vapourismo/hrel"]
					"hrel"

-- | Search form
searchForm :: Maybe T.Text -> Html ()
searchForm mbSearchTerm =
	form_ [class_ "search-form", action_ "/search"] $ do
		input_ $ case mbSearchTerm of
			Just searchTerm ->
				[class_ "input", name_ "q", type_ "text", autofocus_, value_ searchTerm]

			Nothing ->
				[class_ "input", name_ "q", type_ "text", autofocus_]

		div_ [class_ "submit-button"] "Search"

-- | Index page
indexPage :: Html ()
indexPage =
	documentBody (searchForm Nothing)

-- | Torrent tesult table
resultBody :: [Torrent] -> Html ()
resultBody [] = do
	div_ [class_ "no-results"] $ do
		div_ "Nothing has been found."
		div_ "Check again later."
resultBody torrents = do
	div_ [class_ "results"] $
		mapM_ resultRow torrents
	where
		generateAddUrl url =
			"https://www.premiumize.me/downloader?magnet="
			<> T.concatMap (T.pack . escapeURIChar isUnescapedInURIComponent) url

		resultRow :: Torrent -> Html ()
		resultRow (Torrent title url) =
			div_ [class_ "result"] $ do
				div_ [class_ "cell title"] (toHtml title)
				a_ [class_ "cell link", target_ "blank", href_ url] "link"
				a_ [class_ "cell add", target_ "blank", href_ (generateAddUrl url)] "add"

-- | Search result page
searchResultPage :: T.Text -> [Torrent] -> Html ()
searchResultPage searchTerm torrents =
	documentBody $ do
		searchForm (Just searchTerm)
		resultBody torrents

-- | Feeds overview
feedsOverviewPage :: [(Int64, T.Text, String)] -> Html ()
feedsOverviewPage feeds =
	documentBody $ do
		searchForm Nothing
		div_ [class_ "feeds"] $
			forM_ feeds $ \ (fid, title, url) ->
				a_ [class_ "feed", href_ ("/feeds/" <> T.pack (show fid))] $ do
					div_ [class_ "cell title"] (toHtml title)
					div_ [class_ "cell url"] (toHtml url)

-- | Feed contents page
feedContentsPage :: [Torrent] -> Html ()
feedContentsPage torrents =
	documentBody $ do
		searchForm Nothing
		resultBody torrents
