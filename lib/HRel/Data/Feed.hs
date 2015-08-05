{-# LANGUAGE OverloadedStrings #-}

module HRel.Data.Feed (
	-- * Feed
	Feed (..),
	insertFeed,
	createFeed,
	findFeed,
	findAllFeeds,
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
insertFeed :: URI -> Action (Maybe Word64)
insertFeed uri =
	insert qry (Only uri)
	where
		qry = "INSERT INTO feeds (url) VALUES (?) \
		       \ ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)"

-- | Create a "Feed" and make sure it exists in the database.
createFeed :: URI -> Action (Maybe Feed)
createFeed uri =
	fmap (\ mbfid -> Feed <$> mbfid <*> pure uri) (insertFeed uri)

-- | Find an existing feed.
findFeed :: Word64 -> Action (Maybe Feed)
findFeed fid = do
	result <- query "SELECT url FROM feeds WHERE id = ? LIMIT 1" (Only fid)
	case result of
		[Only uri] -> pure (Just (Feed fid uri))
		_          -> pure Nothing

-- | Find all existing feeds.
findAllFeeds :: Action [Feed]
findAllFeeds =
	map (uncurry Feed) <$> query_ "SELECT id, url FROM feeds"
