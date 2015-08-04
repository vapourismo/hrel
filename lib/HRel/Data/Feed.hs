{-# LANGUAGE OverloadedStrings #-}

module HRel.Data.Feed (
	-- * Feed
	Feed (..),
	insertFeed,
	createFeed,
	findFeed,
) where

import           Data.Word

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
