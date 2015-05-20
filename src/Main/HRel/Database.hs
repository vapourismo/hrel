{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HRel.Database (
	-- * Database
	M.Connection,
	withDatabase,

	-- * Conduits
	watchRelease,
	pairTorrents,

	-- * Cleanup
	updateNewTorrents
) where

import Control.Monad
import Control.Monad.Reader
import Control.Exception

import Data.Word
import Data.Maybe

import Data.Conduit
import qualified Data.Conduit.List as C

import qualified Database.MySQL.Simple as M
import qualified Database.MySQL.Simple.QueryParams as M

import HRel.Release
import HRel.Torrent

-- | Do something with a database connection.
withDatabase :: (M.Connection -> IO a) -> IO a
withDatabase =
	bracket (M.connect M.defaultConnectInfo { M.connectDatabase = "hrel" })
	        M.close

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

-- | Pair "Torrent" with its "Release".
pairTorrents :: (MonadIO m) => M.Connection -> Sink Torrent m ()
pairTorrents db =
	C.mapM_ (void . liftIO . insertTorrent) =$= C.sinkNull
	where
		insertTorrent (Torrent _ [] _) = pure []
		insertTorrent (Torrent release uris mbSize) = do
			mWatchID <- findWatchedRelease db release
			case mWatchID of
				Nothing -> pure []
				Just watchID ->
					pure . catMaybes <=< forM uris $ \ uri ->
						insert db
							"INSERT INTO torrents (torrentWatch, torrentURI, torrentContentSize) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE torrentID = LAST_INSERT_ID(torrentID)"
							(watchID, show uri, mbSize)


-- | Insert a "Release" to the watchlist.
watchRelease :: (MonadIO m) => M.Connection -> Conduit Release m Release
watchRelease db =
	C.mapMaybeM $ \ rel -> (rel <$) <$> do
		liftIO $
			insert db
				"INSERT INTO watchlist (watchRelease) VALUES (?) ON DUPLICATE KEY UPDATE watchID = LAST_INSERT_ID(watchID)"
				(M.Only (toText rel))

-- | Update the newly added "Torrent"s.
updateNewTorrents :: M.Connection -> IO ()
updateNewTorrents db =
	void (M.execute_ db "UPDATE watchlist, torrents SET torrentNewlyAdded = 0, watchUpdateTime = NOW() WHERE torrentWatch = watchID AND torrentNewlyAdded = 1")
