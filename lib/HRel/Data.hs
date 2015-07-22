{-# LANGUAGE OverloadedStrings #-}

module HRel.Data (
	-- * Feed
	Feed (..),
	insertFeed,
	createFeed,
	findFeed,

	-- * Release
	Release (..),
	insertRelease,
	createRelease,
	findRelease,

	-- * Extra
	connectReleaseToFeed
) where

import           Data.Int
import           Data.Word
import qualified Data.Text     as T

import           Network.URI   hiding (query)

import           HRel.Database

-- |
data Feed = Feed {
	feedID  :: Word64,
	feedURI :: URI
} deriving (Show, Eq, Ord)

-- |
insertFeed :: URI -> Action (Maybe Word64)
insertFeed uri =
	insert qry (Only uri)
	where
		qry = "INSERT INTO feeds (url) VALUES (?) \
		       \ ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)"

-- |
createFeed :: URI -> Action (Maybe Feed)
createFeed uri =
	fmap (\ mbfid -> Feed <$> mbfid <*> pure uri) (insertFeed uri)

-- |
findFeed :: Word64 -> Action (Maybe Feed)
findFeed fid = do
	result <- query "SELECT url FROM feeds WHERE id = ? LIMIT 1" (Only fid)
	case result of
		[Only uri] -> pure (Just (Feed fid uri))
		_          -> pure Nothing

-- |
data Release = Release {
	releaseID   :: Word64,
	releaseName :: T.Text
} deriving (Show, Eq, Ord)

-- |
insertRelease :: T.Text -> Action (Maybe Word64)
insertRelease rel =
	insert qry (Only rel)
	where
		qry = "INSERT INTO releases (name) VALUES (?) \
		       \ ON DUPLICATE KEY UPDATE id          = LAST_INSERT_ID(id), \
		       \                         updateTime  = CURRENT_TIMESTAMP, \
		       \                         updateCount = updateCount + 1"

-- |
createRelease :: T.Text -> Action (Maybe Release)
createRelease rel =
	fmap (\ mbrid -> Release <$> mbrid <*> pure rel) (insertRelease rel)

-- |
findRelease :: Word64 -> Action (Maybe Release)
findRelease rid = do
	result <- query "SELECT url FROM releases WHERE id = ? LIMIT 1" (Only rid)
	case result of
		[Only rel] -> pure (Just (Release rid rel))
		_          -> pure Nothing

-- |
connectReleaseToFeed :: Word64 -> Word64 -> Action Int64
connectReleaseToFeed rid fid =
	execute qry (fid, rid, fid, rid, fid, rid)
	where
		qry = "INSERT IGNORE INTO feed_contents (feed, rel) \
		       \  SELECT ?, ? FROM dual \
		       \    WHERE EXISTS (SELECT * FROM feeds f WHERE f.id = ?) \
		       \      AND EXISTS (SELECT * FROM releases r WHERE r.id = ?) \
		       \      AND NOT EXISTS (SELECT * FROM feed_contents WHERE feed = ? AND rel = ?)"
