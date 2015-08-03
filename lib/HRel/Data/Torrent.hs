module HRel.Data.Torrent (
	-- * Torrent
	TorrentInfo (..)
) where

import           Network.URI
import           HRel.Data.Release

-- | TorrentInfo
data TorrentInfo = TorrentInfo {
	torrentName        :: ReleaseName,
	torrentSource      :: URI,
	torrentContentSize :: Maybe Word
} deriving (Show, Eq, Ord)
