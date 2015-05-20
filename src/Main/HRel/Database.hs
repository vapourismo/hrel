{-# LANGUAGE OverloadedStrings #-}

module HRel.Database (
	withDatabase,

	-- * Retrieval
	findWatchedRelease,

	-- * Insertion
	InsertID,
	insert,

	watchRelease,
	insertTorrent,

	-- * Updates
	updateNewTorrents
) where

import Control.Monad
import Control.Exception

import Data.Word
import Data.Maybe

import qualified Database.MySQL.Simple as M
import qualified Database.MySQL.Simple.QueryParams as M

import HRel.Release
import HRel.Torrent

-- | Do something with a database connection.
withDatabase :: (M.Connection -> IO a) -> IO a
withDatabase =
	bracket (M.connect M.defaultConnectInfo { M.connectDatabase = "hrel" })
	        (M.close)

-- | Find a "Release" from the watchlist
findWatchedRelease :: M.Connection -> Release -> IO (Maybe InsertID)
findWatchedRelease db release =
	fmap M.fromOnly . listToMaybe <$> M.query db
		"SELECT watchID FROM watchlist WHERE watchRelease = ? LIMIT 1" (M.Only (toText release))

-- | An inserted ID
type InsertID = Word64

-- | Insert something into the database.
insert :: (M.QueryParams q) => M.Connection -> M.Query -> q -> IO (Maybe InsertID)
insert db qry param = do
	rows <- M.execute db qry param
	if rows > 0 then
		Just <$> M.insertID db
	else
		pure Nothing

-- | Insert a "Release" to the watchlist.
watchRelease :: M.Connection -> Release -> IO (Maybe InsertID)
watchRelease db release =
	insert db
		"INSERT INTO watchlist (watchRelease) VALUES (?) ON DUPLICATE KEY UPDATE watchID = LAST_INSERT_ID(watchID)"
		(M.Only (toText release))

-- | Insert a "Torrent".
insertTorrent :: M.Connection -> Torrent -> IO [InsertID]
insertTorrent _ (Torrent _ [] _) = pure []
insertTorrent db (Torrent release uris mbSize) = do
	mWatchID <- findWatchedRelease db release
	case mWatchID of
		Nothing -> pure []
		Just watchID ->
			pure . catMaybes <=< forM uris $ \ uri ->
				insert db
					"INSERT INTO torrents (torrentWatch, torrentURI, torrentContentSize) VALUES ()"
					(watchID, show uri, mbSize)

-- | Update the newly added "Torrent"s.
updateNewTorrents :: M.Connection -> IO ()
updateNewTorrents db =
	void (M.execute_ db "UPDATE watchlist, torrents SET torrentNewlyAdded = 0, watchUpdateTime = NOW() WHERE torrentWatch = watchID AND torrentNewlyAdded = 1")
