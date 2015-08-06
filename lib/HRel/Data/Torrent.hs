{-# LANGUAGE OverloadedStrings #-}
module HRel.Data.Torrent (
	-- * Torrent
	TorrentInfo (..),
	Torrent (..),
	insertTorrent,
	createTorrent,
	addTorrent
) where

import           Data.Word

import           Network.URI

import           HRel.Database
import           HRel.Data.Release

-- | TorrentInfo
data TorrentInfo = TorrentInfo {
	torrentInfoName        :: ReleaseName,
	torrentInfoSource      :: URI,
	torrentInfoContentSize :: Maybe Word
} deriving (Show, Eq, Ord)

-- | Torrent
data Torrent = Torrent {
	torrentID   :: Word64,
	torrentInfo :: TorrentInfo
} deriving (Show, Eq, Ord)

-- |
insertTorrent :: TorrentInfo -> Action Word64
insertTorrent (TorrentInfo name uri mbSize) =
	insert qry (uri, getReleaseName name, mbSize)
	where
		qry = "INSERT INTO torrents (uri, name, size) VALUES (?, ?, ?) \
		       \ ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)"

-- |
createTorrent :: TorrentInfo -> Action Torrent
createTorrent info =
	fmap (\ tid -> Torrent tid info) (insertTorrent info)

-- |
addTorrent :: Release -> Torrent -> Action ()
addTorrent rel tor =
	() <$ execute "INSERT IGNORE INTO release_links (rel, tor) VALUES (?, ?)" (releaseID rel, torrentID tor)
