{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.Feeds (
	-- * Atom Feeds
	atomFeedFilter,
	parseAtomFeed,
	fetchAtomFeed
) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding   as T

import           Network.HTTP.Client
import           Network.HTTP.Types.Status

import           HRel.Markup
import           HRel.Data.Release

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
	case parseUrl url of
		Just req -> fetchAtomFeed' mgr req
		Nothing  -> pure Nothing

-- | Fetches the release names contained in an Atom feed.
fetchAtomFeed' :: Manager -> Request -> IO (Maybe [ReleaseName])
fetchAtomFeed' mgr req = do
	res <- httpLbs req mgr
	pure $ case responseStatus res of
		Status 200 _ -> parseAtomFeed (BL.toStrict (responseBody res))
		_            -> Nothing
