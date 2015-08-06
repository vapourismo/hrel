{-# LANGUAGE OverloadedStrings #-}

module HRel.Data.Release (
	-- * Release name,
	ReleaseName,
	getReleaseName,
	normalizeReleaseName,

	-- * Release
	Release (..),
	insertRelease,
	createRelease,
	findRelease,
	addRelease
) where

import           Data.Char
import qualified Data.Text     as T
import           Data.Word

import           HRel.Database
import           HRel.Data.Feed

-- | Release identfier name
newtype ReleaseName = ReleaseName { getReleaseName :: T.Text }
	deriving (Eq, Ord)

instance Show ReleaseName where
	show = show . getReleaseName

-- | Normalize the release name.
normalizeReleaseName :: T.Text -> ReleaseName
normalizeReleaseName =
	normalizeName . fst . retrieveAuthor
	where
		splitProperly f = filter (not . T.null) . map T.strip . T.split f

		retrieveAuthor txt =
			case splitProperly (== '-') txt of
				[rn] -> (rn, Nothing)
				xs   -> (T.intercalate "-" (init xs),
				         Just (fst (T.span (not . isSpace) (last xs))))

		normalizeName =
			ReleaseName . T.toLower . T.intercalate " " . splitProperly (not . isAlphaNum)

-- | Release
data Release = Release {
	releaseID   :: Word64,
	releaseName :: ReleaseName
} deriving (Show, Eq, Ord)

-- |
insertRelease :: ReleaseName -> Action Word64
insertRelease (ReleaseName rel) =
	insert qry (Only rel)
	where
		qry = "INSERT INTO releases (name) VALUES (?) \
		       \ ON DUPLICATE KEY UPDATE id          = LAST_INSERT_ID(id), \
		       \                         updateTime  = CURRENT_TIMESTAMP, \
		       \                         updateCount = updateCount + 1"

-- |
createRelease :: ReleaseName -> Action Release
createRelease rel =
	fmap (\ rid -> Release rid rel) (insertRelease rel)

-- |
findRelease :: Word64 -> Action Release
findRelease rid = do
	fmap (\ (Only rel) -> Release rid (ReleaseName rel))
	     (query1 "SELECT url FROM releases WHERE id = ? LIMIT 1" (Only rid))

-- | Attach a release to a feed.
addRelease :: Feed -> Release -> Action ()
addRelease feed rel =
	() <$ execute "INSERT IGNORE INTO feed_contents (feed, rel) VALUES (?, ?)"
	              (feedID feed, releaseID rel)
