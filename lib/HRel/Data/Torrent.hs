module HRel.Data.Torrent (
	-- * Torrent
	TorrentInfo (..)
) where

import           HRel.Data.Release
import           Network.URI

-- | TorrentInfo
data TorrentInfo = TorrentInfo {
	torrentName               :: ReleaseName,
	torrentSource      ::     URI,
	torrentContentSize :: Maybe Word
} deriving (Show, Eq, Ord)
