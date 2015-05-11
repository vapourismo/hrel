{-# LANGUAGE OverloadedStrings #-}

module HRel.Database (
	withDatabase,

	InsertID,
	insertRelease,
	insertTorrent,

	findTorrents,
) where

import Control.Exception

import Data.Word
import Data.Maybe
import qualified Database.MySQL.Simple as M

import Network.URI

import HRel.Source

-- | Do something with a database connection.
withDatabase :: (M.Connection -> IO a) -> IO a
withDatabase =
	bracket (M.connect M.defaultConnectInfo { M.connectDatabase = "hrel" })
	        (M.close)

-- | An inserted ID
type InsertID = Word64

-- | Insert a release into the database.
insertRelease :: M.Connection -> Release -> IO InsertID
insertRelease db release = do
	M.execute db "INSERT INTO releases (releaseName) VALUES (?) ON DUPLICATE KEY UPDATE releaseID = LAST_INSERT_ID(releaseID)"
	             (M.Only (toText release))
	M.insertID db

-- | Insert a torrent into the database.
insertTorrent :: M.Connection -> Torrent -> IO InsertID
insertTorrent db (Torrent release uris size) = do
	releaseID <- insertRelease db release
	M.executeMany db "INSERT INTO torrents (torrentRelease, torrentURI, torrentSize) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE torrentID = LAST_INSERT_ID(torrentID)"
	                 (map (\ uri -> (releaseID, show uri, size)) uris)
	M.insertID db

-- |
findTorrents :: M.Connection -> Release -> IO [Torrent]
findTorrents db release =
	map make <$> M.query db "SELECT releaseName, torrentURI, torrentSize FROM torrents, releases WHERE torrentRelease = releaseID AND releaseName = ? GROUP BY torrentURI"
	                        (M.Only (toText release))
	where
		make (releaseName, torrentURI, torrentSize) =
			Torrent (makeRelease releaseName) [fromJust (parseURI torrentURI)] torrentSize

