{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Catch

import           Data.Word
import           Data.Conduit
import qualified Data.Conduit.List as C

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           HRel.Source.Feeds
import           HRel.Source.KickAssTorrents
import           HRel.Release
import           HRel.Torrent
import           HRel.Database

trackReleases :: (MonadIO m) => Database -> Conduit (Word64, Release) m (Word64, Release)
trackReleases db =
	C.mapMaybeM $ \ (fid, rel) ->
		runAction db $ do
			mbReleaseID <- insert "INSERT INTO releases (feed, name, track) values (?, ?, true) ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)" (fid, fromRelease rel)
			case mbReleaseID of
				Just releaseID -> do
					mbTracks <- query "SELECT track FROM releases WHERE id = ?" (Only releaseID)

					pure $ case mbTracks of
						[Only True] -> Just (releaseID, rel)
						_           -> Nothing

				Nothing ->
					pure Nothing

findTorrents :: (MonadIO m, MonadThrow m) => Manager -> Conduit (Word64, Release) m (Word64, Torrent)
findTorrents mgr =
	C.concatMapM $ \ (rid, rel) -> do
		torrents <- yield rel $$ kickAssSearch mgr =$= C.consume
		pure (map ((,) rid) torrents)

trackTorrents :: (MonadIO m) => Database -> Sink (Word64, Torrent) m ()
trackTorrents db =
	C.mapM_ $ \ (rid, torrent) ->
		runAction db $
			forM_ (torrentSource torrent) $ \ src ->
				execute "INSERT INTO torrents (rel, url, size) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)" (rid, show src, torrentContentSize torrent)

main :: IO ()
main = withDatabase $ \ db -> withManager tlsManagerSettings $ \ mgr -> do
	feeds <- runAction db (query_ "SELECT id, url FROM feeds")

	runConduit $
		mapM_ (\ (fid, url) -> fromRSSTitles mgr url =$= C.map ((,) fid)) feeds
			=$= trackReleases db
			=$= findTorrents mgr
			=$= trackTorrents db
