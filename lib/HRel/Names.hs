{-# LANGUAGE OverloadedStrings #-}

module HRel.Names (
	parseTags,
	stripGroup
) where

import           Data.Char
import           Data.List
import qualified Data.Text as T

-- | Retrieve tags within the name without sorting out the qualifiers.
parseTags :: T.Text -> [T.Text]
parseTags name =
	nub (filter (not . T.null) (T.split (not . isAlphaNum) (T.toLower (stripGroup name))))

-- | Remove the '-ABC' part in the end of release names.
stripGroup :: T.Text -> T.Text
stripGroup name =
	T.dropWhileEnd (== '-') (T.dropWhileEnd (/= '-') name)
