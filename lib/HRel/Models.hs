module HRel.Models (
	Release (..),
	Torrent (..)
) where

import qualified Data.Text as T

-- |
data Release = Release {
	releaseName :: T.Text
} deriving (Show, Eq, Ord)

-- |
data Torrent = Torrent {
	torrentTitle :: T.Text,
	torrentURI   :: T.Text
} deriving (Show, Eq, Ord)
