{-# LANGUAGE OverloadedStrings #-}

module HRel.Release (
	-- * Release
	Release,
	fromRelease,
	makeRelease,
	normalizeRelease
) where

import           Data.Char
import qualified Data.Text as T

-- | Release Identifier
newtype Release = Release { fromRelease :: T.Text }
	deriving (Eq, Ord)

instance Show Release where
	show = T.unpack . fromRelease

-- | Make a release from the given raw text.
makeRelease :: T.Text -> Release
makeRelease = Release . normalizeRelease

-- | Normalize the release name.
normalizeRelease :: T.Text -> T.Text
normalizeRelease =
	normalizeName . fst . retrieveAuthor
	where
		splitProperly f = filter (not . T.null) . map T.strip . T.split f

		retrieveAuthor txt =
			case splitProperly (== '-') txt of
				[rn] -> (rn, Nothing)
				xs   -> (T.intercalate "-" (init xs),
				         Just (fst (T.span (not . isSpace) (last xs))))

		normalizeName =
			T.toLower . T.intercalate " " . splitProperly (not . isAlphaNum)
