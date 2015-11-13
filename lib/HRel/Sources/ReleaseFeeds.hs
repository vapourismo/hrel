{-# LANGUAGE OverloadedStrings #-}

module HRel.Sources.ReleaseFeeds (
	parseAtomReleaseFeed,
	parseRSSReleaseFeed,
	parseReleaseFeed
) where

import           Control.Applicative

import qualified Data.ByteString    as B
import qualified Data.Text.Encoding as T

import           HRel.Sources.Markup
import           HRel.Sources.ReleaseName

-- | Filter for Atom feeds.
atomFeedFilter :: NodeFilter B.ByteString [ReleaseName]
atomFeedFilter =
	relativeTag "feed" (foreachTag "entry" (forTag "title" titleFilter))
	where
		titleFilter =
			normalizeReleaseName . T.decodeUtf8 <$> text

-- | Filter for RSS feeds.
rssFeedFilter :: NodeFilter B.ByteString [ReleaseName]
rssFeedFilter =
	relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
		forTag "title" (normalizeReleaseName . T.decodeUtf8 <$> text)

-- | Parse the contents of an Atom feed in order to extract the release names from entry titles.
parseAtomReleaseFeed :: B.ByteString -> Maybe [ReleaseName]
parseAtomReleaseFeed contents =
	runNodeFilter atomFeedFilter (fromMarkup' contents)

-- | Parse the contents of an RSS feed in order to extract the release names from entry titles.
parseRSSReleaseFeed :: B.ByteString -> Maybe [ReleaseName]
parseRSSReleaseFeed contents =
	runNodeFilter rssFeedFilter (fromMarkup' contents)

-- | Parse the contents of a RSS or Atom feed.
parseReleaseFeed :: B.ByteString -> Maybe [ReleaseName]
parseReleaseFeed contents =
	runNodeFilter (rssFeedFilter <|> atomFeedFilter) (fromMarkup' contents)
