module HRel.Release (
	Release (..),
	Product (..)
) where

import Data.Word
import Data.Text

-- | Release information
data Release = Release Text Product
	deriving (Show, Eq, Ord)

-- |
data Product
	= Movie Text
	| Episode Text Word Word
	| Game Text
	| Unknown
	deriving (Show, Eq, Ord)
