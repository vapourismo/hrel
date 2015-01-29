module HRel.Release (
	Release (..)
) where

import Data.Word
import qualified Data.Text.Lazy as T

-- | Release information
data Release
	= Movie T.Text T.Text
	| Television T.Text T.Text Word Word
	| Game T.Text T.Text
	| Unknown T.Text T.Text
	deriving (Show, Eq)
