{-# LANGUAGE OverloadedStrings #-}

module HRel.Data.Feed (
	-- * Feed
	Feed (..),
	insertFeed,
	createFeed,
	findFeed,
	findFeedByURI,
	findAllFeeds
) where

import           Data.Word

import           Network.URI   hiding (query)

import           HRel.Database

-- | Feed
data Feed = Feed {
	feedID  :: Word64,
	feedURI :: URI
} deriving (Show, Eq, Ord)

-- | Insert a feed URI, but do not instantiate a "Feed".
insertFeed :: URI -> Action Word64
insertFeed uri =
	insert qry (Only uri)
	where
		qry = "INSERT INTO feeds (url) VALUES (?) \
		       \ ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)"

-- | Create a "Feed" and make sure it exists in the database.
createFeed :: URI -> Action Feed
createFeed uri =
	fmap (flip Feed uri) (insertFeed uri)

-- | Find an existing feed.
findFeed :: Word64 -> Action Feed
findFeed fid = do
	fmap (\ (Only uri) -> Feed fid uri)
	     (query1 "SELECT url FROM feeds WHERE id = ? LIMIT 1" (Only fid))

-- | Find an existing feed using its URI.
findFeedByURI :: URI -> Action Feed
findFeedByURI uri = do
	fmap (\ (Only fid) -> Feed fid uri)
	     (query1 "SELECT id FROM feeds WHERE url = ? LIMIT 1" (Only uri))

-- | Find all existing feeds.
findAllFeeds :: Action [Feed]
findAllFeeds =
	map (uncurry Feed) <$> query_ "SELECT id, url FROM feeds"
