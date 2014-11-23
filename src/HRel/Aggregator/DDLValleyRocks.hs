{-# LANGUAGE OverloadedStrings #-}

module HRel.Aggregator.DDLValleyRocks (
	aggregate
) where

import Data.List
import qualified Data.Text.Lazy as T

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Network.URI

import HRel.Markup
import HRel.Download
import HRel.Tools

-- | Use only links to these hosters.
validHostNames :: [String]
validHostNames = ["ul.to", "uploaded.to", "uploaded.net",
                  "uptobox.com", "rapidgator.net", "go4up.com"]

-- | Filter a blog post.
postFilter :: NodeFilter T.Text [URI]
postFilter =
	relativeTag "div" $ do
		attrDiv <- attr "class"
		guard (attrDiv == "cont cl")
		relativeTags "a" $ do
			attrA <- attr "class"
			guard (attrA == "ext-link")

			href <- fmap trimText $ attr "href"
			inner <- fmap trimText $ text
			guard (inner == href)

			uri <- toURI href
			hoster <- toHost uri
			guard (elem hoster validHostNames)

			return uri

-- | Filter a RSS feed.
rssFilter :: NodeFilterT T.Text IO [(,) [T.Text] [URI]]
rssFilter =
	relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
		(,) <$> fmap (map trimText . T.split (== '&')) (forTag "title" text)
		    <*> (mergeLinks <$> (forTag "link" text
		                         >>= liftIO . aggregatePost . T.unpack . trimText)
		                    <*> foreachTag "enclosure" (attr "url" >>= toURI))
	where
		mergeLinks a b = nub (a ++ b)

-- | Aggregate the links in a blog post.
aggregatePost :: String -> IO [URI]
aggregatePost =
	fmap (maybe [] id . runNodeFilter postFilter) . downloadNode

-- | Aggregate links (including blog post) from a RSS feed.
aggregate :: String -> IO [(,) [T.Text] [URI]]
aggregate loc = do
	cnts <- downloadNode loc
	res <- runNodeFilterT rssFilter cnts
	return (maybe [] id res)
