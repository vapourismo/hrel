{-# LANGUAGE OverloadedStrings #-}

module HRel.Sources.ReleaseName (
	ReleaseName,
	normalizeReleaseName
) where

import           Data.Char
import qualified Data.Text as T

-- | Release identfier name
newtype ReleaseName = ReleaseName {
	fromReleaseName :: T.Text
} deriving (Eq, Ord)

instance Show ReleaseName where
	show = show . fromReleaseName

-- | Normalize the release name.
normalizeReleaseName :: T.Text -> ReleaseName
normalizeReleaseName =
	normalizeName . fst . retrieveAuthor
	where
		splitProperly f = filter (not . T.null) . map T.strip . T.split f

		retrieveAuthor txt =
			case splitProperly (== '-') txt of
				[]   -> (T.empty, Nothing)
				[rn] -> (rn, Nothing)
				xs   -> (T.intercalate "-" (init xs),
				         Just (fst (T.span (not . isSpace) (last xs))))

		normalizeName =
			ReleaseName . T.toLower . T.intercalate " " . splitProperly (not . isAlphaNum)
