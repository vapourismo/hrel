{-# LANGUAGE OverloadedStrings #-}

module HRel.Names (
	parseTags,
	stripReleaseGroup
) where

import           Data.Char
import           Data.List
import qualified Data.Text as T

-- | Retrieve tags within the name without sorting out the qualifiers.
parseTags :: T.Text -> [T.Text]
parseTags name =
	nub (filter (not . T.null) (T.split (not . isAlphaNum) (T.toLower (stripReleaseGroup name))))

-- | Remove the '-ABC' part in the end of release names.
stripReleaseGroup :: T.Text -> T.Text
stripReleaseGroup txt =
	T.dropWhileEnd (== '-') (T.dropWhileEnd (/= '-') txt)
