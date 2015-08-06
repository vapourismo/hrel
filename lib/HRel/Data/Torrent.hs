{-# LANGUAGE OverloadedStrings #-}
module HRel.Data.Torrent (
	-- * Torrent
	TorrentInfo (..),
	Torrent (..),
	insertTorrent,
	insertTorrents,
	createTorrent,
	addTorrent,
	addMatchingTorrents
) where

import           Data.Int
import           Data.Word
import qualified Data.Text         as T

import           Network.URI

import           HRel.Database
import           HRel.Data.Release

-- | TorrentInfo
data TorrentInfo = TorrentInfo {
	torrentInfoName        :: T.Text,
	torrentInfoNormalized  :: ReleaseName,
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
insertTorrent (TorrentInfo name normalized uri mbSize) =
	insert qry (uri, name, getReleaseName normalized, mbSize)
	where
		qry = "INSERT INTO torrents (uri, name, normalized, size) VALUES (?, ?, ?, ?) \
		       \ ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)"

-- |
insertTorrents :: [TorrentInfo] -> Action Int64
insertTorrents infos =
	executeMany qry (map row infos)
	where
		row (TorrentInfo name normalized uri mbSize) =
			(uri, name, getReleaseName normalized, mbSize)

		qry = "INSERT INTO torrents (uri, name, normalized, size) VALUES (?, ?, ?, ?) \
		       \ ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)"

-- |
createTorrent :: TorrentInfo -> Action Torrent
createTorrent info =
	fmap (\ tid -> Torrent tid info) (insertTorrent info)

-- |
addTorrent :: Release -> Torrent -> Action ()
addTorrent rel tor =
	() <$ execute "INSERT IGNORE INTO release_links (rel, tor) VALUES (?, ?)" (releaseID rel, torrentID tor)

-- |
addMatchingTorrents :: Action Int64
addMatchingTorrents =
	execute_ "INSERT IGNORE INTO release_links (rel, tor) \
	          \ SELECT r.id, t.id from releases r, torrents t WHERE t.normalized = r.name"
