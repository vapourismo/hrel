{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.RSSFeed (
	-- * RSS Feeds
	rssFeedFilter,
	parseRSSFeed,
	fetchRSSFeed
) where

import qualified Data.ByteString    as B
import qualified Data.Text.Encoding as T

import           HRel.Data.Release
import           HRel.HTTP
import           HRel.Markup

-- | Filter for RSS feeds.
rssFeedFilter :: NodeFilter B.ByteString [ReleaseName]
rssFeedFilter =
	relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
		forTag "title" (normalizeReleaseName . T.decodeUtf8 <$> text)

-- | Parse the contents of an RSS feed in order to extract the release names from entry titles.
parseRSSFeed :: B.ByteString -> Maybe [ReleaseName]
parseRSSFeed contents =
	runNodeFilter rssFeedFilter (fromMarkup' contents)

-- | Fetches the release names contained in an RSS feed.
fetchRSSFeed :: Manager -> String -> IO (Maybe [ReleaseName])
fetchRSSFeed mgr url =
	(>>= parseRSSFeed) <$> download mgr url
