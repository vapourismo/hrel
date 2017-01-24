{-# LANGUAGE OverloadedStrings #-}

module HRel.Web.Templates (
	lucid,
	indexPage,
	searchResultPage
) where

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

-- | Search result page
searchResultPage :: T.Text -> [Torrent] -> Html ()
searchResultPage searchTerm torrents =
	documentBody $ do
		searchForm (Just searchTerm)

		if null torrents then
			div_ [class_ "no-results"] $ do
				div_ "Nothing has been found."
				div_ "Check again later."
		else
			div_ [class_ "results"] $
				mapM_ searchResultRow torrents

	where
		generateAddUrl url =
			"https://www.premiumize.me/downloader?magnet="
			<> T.concatMap (T.pack . escapeURIChar isUnescapedInURIComponent) url

		searchResultRow :: Torrent -> Html ()
		searchResultRow (Torrent title url) =
			div_ [class_ "result"] $ do
				div_ [class_ "cell title"] (toHtml title)
				a_ [class_ "cell link", target_ "blank", href_ url] "link"
				a_ [class_ "cell add", target_ "blank", href_ (generateAddUrl url)] "add"
