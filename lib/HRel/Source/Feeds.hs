{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.Feeds (
	-- * Atom Feeds
	parseAtomFeed,
	parseAtomFeedRequest,
	parseAtomFeedURL,
) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding   as T

import           Network.HTTP.Client
import           Network.HTTP.Types.Status

import           HRel.Markup
import           HRel.Data.Release

-- | Parse the contents of an Atom feed in order to extract the release names from entry titles.
parseAtomFeed :: B.ByteString -> Maybe [ReleaseName]
parseAtomFeed contents =
	runNodeFilter rootFilter (fromMarkup' contents)
	where
		titleFilter =
			normalizeReleaseName . T.decodeUtf8 <$> text

		rootFilter =
			reverse <$> relativeTag "feed" (foreachTag "entry" (forTag "title" titleFilter))

-- | Same as "parseAtomFeed" but does the download also.
parseAtomFeedRequest :: Manager -> Request -> IO (Maybe [ReleaseName])
parseAtomFeedRequest mgr req = do
	res <- httpLbs req mgr
	case responseStatus res of
		Status 200 _ -> pure (parseAtomFeed (BL.toStrict (responseBody res)))
		_            -> pure Nothing

-- | Same as "parseAtomFeedRequest" but generates the request for you.
parseAtomFeedURL :: Manager -> String -> IO (Maybe [ReleaseName])
parseAtomFeedURL mgr url =
	case parseUrl url of
		Just req -> parseAtomFeedRequest mgr req
		Nothing  -> pure Nothing
