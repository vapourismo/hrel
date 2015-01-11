{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.OneClickWatch where

import Data.List
import Data.Char
import qualified Data.Text.Lazy as T

import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import Network.URI

import HRel.Markup.Node
import HRel.Markup.Download
import HRel.URI

-- | Filter a blog post.
postFilter :: NodeFilter T.Text [URI]
postFilter =
	relativeTag "div" $ do
		attrGuard "id" "content"
		forTag "div" . forTag "div" $ do
			attrGuard "class" "entry"
			forTag "p" . relativeTags "a" $ do
				href <- attr "href"
				inner <- text
				guard (T.strip inner == T.strip href)

				toURI (T.strip href)

-- | Filter a RSS feed.
rssFilter :: NodeFilterT T.Text IO [(,) T.Text [URI]]
rssFilter =
	relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
		appl <$> forTag "title" text
		     <*> (forTag "link" text >>= aggregateLinks)
	where
		appl t l =
			(T.intercalate "." (filter (not . T.null) (T.split isSpace t)),
			 nub (maybe [] id l))
		aggregateLinks =
			liftIO . withNodeFilter' postFilter . T.unpack . T.strip

---- | Trim space around a text.
--T.strip :: T.Text -> T.Text
--T.strip = T.dropAround isSpace

---- | Aggregate the links in a blog post.
--aggregatePost :: String -> IO [URI]
--aggregatePost =
--	fmap (maybe [] id . runNodeFilter postFilter) . downloadNode

---- | Aggregate links (including blog post) from a RSS feed.
--aggregate :: IO [(,) T.Text [URI]]
--aggregate = do
--	cnts <- downloadNode "http://oneclickwatch.ws/feed/"
--	res <- runNodeFilterT rssFilter cnts
--	return (maybe [] id res)
