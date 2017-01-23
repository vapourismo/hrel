{-# LANGUAGE OverloadedStrings #-}

module HRel.Names (
	parseTags
) where

import           Data.Char
import           Data.List
import qualified Data.Text as T

-- | Retrieve tags within the name without sorting out the qualifiers.
parseTags :: T.Text -> [T.Text]
parseTags name =
	nub (filter (not . T.null) (T.split (not . isAlphaNum) (T.toLower name)))
