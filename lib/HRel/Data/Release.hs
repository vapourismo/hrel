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
	findRelease
) where

import           Data.Int
import           Data.Word
import           Data.Char
import qualified Data.Text     as T

import           Network.URI   hiding (query)

import           HRel.Database

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
insertRelease :: ReleaseName -> Action (Maybe Word64)
insertRelease (ReleaseName rel) =
	insert qry (Only rel)
	where
		qry = "INSERT INTO releases (name) VALUES (?) \
		       \ ON DUPLICATE KEY UPDATE id          = LAST_INSERT_ID(id), \
		       \                         updateTime  = CURRENT_TIMESTAMP, \
		       \                         updateCount = updateCount + 1"

-- |
createRelease :: ReleaseName -> Action (Maybe Release)
createRelease rel =
	fmap (\ mbrid -> Release <$> mbrid <*> pure rel) (insertRelease rel)

-- |
findRelease :: Word64 -> Action (Maybe Release)
findRelease rid = do
	result <- query "SELECT url FROM releases WHERE id = ? LIMIT 1" (Only rid)
	case result of
		[Only rel] -> pure (Just (Release rid (ReleaseName rel)))
		_          -> pure Nothing
