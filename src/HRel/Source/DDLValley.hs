{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.DDLValley where

import Data.List
import qualified Data.Text.Lazy as T

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import HRel.Markup.Node
import HRel.Markup.Download
import HRel.URI

-- | Filter a blog post.
postFilter :: NodeFilter T.Text [URI]
postFilter =
	relativeTag "div" $ do
		attrGuard "class" "cont cl"
		relativeTags "a" $ do
			attrGuard "class" "ext-link"

			href <- attr "href"
			inner <- text
			guard (T.strip inner == T.strip href)

			toURI (T.strip href)

-- | Filter a RSS feed.
rssFilter :: NodeFilterT T.Text IO [(,) [T.Text] [URI]]
rssFilter =
	relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
		appl <$> forTag "title" text
		     <*> (forTag "link" text >>= aggregateLinks)
		     <*> foreachTag "enclosure" (attr "url" >>= toURI)
	where
		appl t l e =
			(map T.strip (T.split (== '&') t),
			 nub (maybe [] id l ++ e))
		aggregateLinks =
			liftIO . withNodeFilter' postFilter . T.unpack . T.strip

-- Example Feeds
--   "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/"
--   "http://www.ddlvalley.rocks/category/tv-shows/web-dl/feed/"
--   "http://www.ddlvalley.rocks/category/movies/bdrip/feed/"
--   "http://www.ddlvalley.rocks/category/movies/bluray-1080p/feed/"
--   "http://www.ddlvalley.rocks/category/movies/bluray-720p/feed/"