{-# LANGUAGE OverloadedStrings #-}

module HRel.Aggregators.OneClickWatch (
	aggregate,
	aggregatePost,
) where

import Data.List
import Data.Char
import qualified Data.Text.Lazy as T

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Network.URI

import HRel.Markup
import HRel.Download
import HRel.Tools

-- | Filter a blog post.
postFilter :: NodeFilter T.Text [URI]
postFilter =
	relativeTag "div" $ do
		attrDiv <- attr "id"
		guard (attrDiv == "content")

		forTag "div" . forTag "div" $ do
			attrDiv' <- attr "class"
			guard (attrDiv' == "entry")

			forTag "p" . relativeTags "a" $ do
				href <- fmap trimText $ attr "href"
				inner <- fmap trimText $ text
				guard (inner == href)

				toURI href

-- | Filter a RSS feed.
rssFilter :: NodeFilterT T.Text IO [(,) T.Text [URI]]
rssFilter =
	relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
		(,) <$> fmap fetchNames (forTag "title" text)
		    <*> (mergeLinks <$> (forTag "link" text >>= aggregateLink)
		                    <*> foreachTag "enclosure" (attr "url" >>= toURI))
	where
		fetchNames = T.concat . intersperse "."
		                      . filter (not . T.null)
		                      . map trimText
		                      . T.split isSpace
		mergeLinks a b = nub (a ++ b)
		aggregateLink = liftIO . aggregatePost . T.unpack . trimText

-- | Aggregate the links in a blog post.
aggregatePost :: String -> IO [URI]
aggregatePost =
	fmap (maybe [] id . runNodeFilter postFilter) . downloadNode

-- | Aggregate links (including blog post) from a RSS feed.
aggregate :: IO [(,) T.Text [URI]]
aggregate = do
	cnts <- downloadNode "http://oneclickwatch.ws/feed/"
	res <- runNodeFilterT rssFilter cnts
	return (maybe [] id res)
