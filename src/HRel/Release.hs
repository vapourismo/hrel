module HRel.Release (
	Release (..),
	Product (..)
) where

import Data.Word
import Data.Text.Lazy

-- | Release information
data Release = Release Text (Maybe Product)
	deriving (Show, Eq, Ord)

-- |
data Product
	= Movie Text
	| Episode Text Word Word
	| Game Text
	deriving (Show, Eq, Ord)
