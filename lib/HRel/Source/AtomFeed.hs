{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.AtomFeed (
	-- * Atom Feeds
	atomFeedFilter,
	parseAtomFeed,
	fetchAtomFeed,
	fetchAtomFeed'
) where

import qualified Data.ByteString           as B
import qualified Data.Text.Encoding        as T

import           HRel.Data.Release
import           HRel.Markup
import           HRel.HTTP

-- | Filter for Atom feeds.
atomFeedFilter :: NodeFilter B.ByteString [ReleaseName]
atomFeedFilter =
	reverse <$> relativeTag "feed" (foreachTag "entry" (forTag "title" titleFilter))
	where
		titleFilter =
			normalizeReleaseName . T.decodeUtf8 <$> text

-- | Parse the contents of an Atom feed in order to extract the release names from entry titles.
parseAtomFeed :: B.ByteString -> Maybe [ReleaseName]
parseAtomFeed contents =
	runNodeFilter atomFeedFilter (fromMarkup' contents)

-- | Fetches the release names contained in an Atom feed.
fetchAtomFeed :: Manager -> String -> IO (Maybe [ReleaseName])
fetchAtomFeed mgr url =
	(>>= parseAtomFeed) <$> download mgr url

-- | Fetches the release names contained in an Atom feed.
fetchAtomFeed' :: Manager -> Request -> IO (Maybe [ReleaseName])
fetchAtomFeed' mgr req = do
	(>>= parseAtomFeed) <$> download' mgr req
