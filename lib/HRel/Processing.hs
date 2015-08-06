{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HRel.Processing (
	-- * Manifest
	Manifest (..),
	withManifest,

	-- * Workers
	WorkerCommand,
	spawnWorkers,
	spawnJobTimer,

	-- * Directives
	queueProcessFeed,
	queueProcessFeedEntry
) where

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.Suspend
import Control.Concurrent.Timer

import HRel.Database
import HRel.Config
import HRel.HTTP

import HRel.Data.Feed
import HRel.Data.Release
import HRel.Data.Torrent

import HRel.Source.AtomFeed
import HRel.Source.KickAssTorrents

data Manifest = Manifest {
	mDatabase :: Database,
	mManager  :: Manager,
	mChannel  :: Chan WorkerCommand
}

withManifest :: (Manifest -> IO a) -> IO a
withManifest action =
	withDatabase (\ db -> Manifest db <$> newTLSManager <*> newChan >>= action)

data WorkerCommand
	= ProcessFeed Feed
	| ProcessFeedEntry Feed ReleaseName
	| ProcessTorrent Release TorrentInfo

spawnWorkers :: Manifest -> IO [ThreadId]
spawnWorkers Manifest {..} = do
	num <- getNumCapabilities
	replicateM (max 1 num) (forkIO worker)
	where
		worker = withDatabase $ \ db ->
			let mf = Manifest db mManager mChannel
			in forever $ do
				msg <- readChan mChannel
				case msg of
					ProcessFeed feed          -> processFeed mf feed
					ProcessFeedEntry feed rel -> processFeedEntry mf feed rel
					ProcessTorrent rel info   -> processTorrentInfo mf rel info

spawnJobTimer :: Manifest -> IO ()
spawnJobTimer mf = do
	repeatedTimer (processAllFeeds mf) (mDelay 20)

	case confHourlyDump of
		Just dump -> void $ do
			repeatedTimer (processHourlyDump mf dump) (hDelay 1)
			forkIO (processHourlyDump mf dump)

		Nothing ->
			pure ()

processHourlyDump :: Manifest -> String -> IO ()
processHourlyDump Manifest {..} url = do
	pure ()
	--mbTors <- fetchKickAssDump mManager url
	--case mbTors of
	--	Just infos -> void $ do
	--		mb <- runAction mDatabase (insertTorrents infos)
	--		case mb of
	--			Just num ->
	--				putStrLn ("processHourlyDump: Inserted " ++ show num ++ "/" ++ show (length infos))

	--			Nothing ->
	--				putStrLn ("processHourlyDump: Failed to insert all of " ++ show (length infos) ++ " received torrents")

	--	Nothing ->
	--		pure ()

processAllFeeds :: Manifest -> IO ()
processAllFeeds mf@(Manifest {..}) =
	runAction mDatabase findAllFeeds >>= maybe (pure ()) (mapM_ (queueProcessFeed mf))

queueProcessFeed :: Manifest -> Feed -> IO ()
queueProcessFeed Manifest {..} feed =
	writeChan mChannel (ProcessFeed feed)

processFeed :: Manifest -> Feed -> IO ()
processFeed mf@(Manifest {..}) feed = do
	putStrLn ("processFeed: " ++ show feed)
	mbRels <- fetchAtomFeed mManager (show (feedURI feed))
	maybe (pure ()) (mapM_ (queueProcessFeedEntry mf feed)) mbRels

queueProcessFeedEntry :: Manifest -> Feed -> ReleaseName -> IO ()
queueProcessFeedEntry Manifest {..} feed rel =
	writeChan mChannel (ProcessFeedEntry feed rel)

processFeedEntry :: Manifest -> Feed -> ReleaseName -> IO ()
processFeedEntry mf@(Manifest {..}) feed name = do
	putStrLn ("processFeedEntry: " ++ show name)
	mbRel <- runAction mDatabase $ do
		rel <- createRelease name
		addRelease feed rel
		pure rel

	maybe (pure ()) (liftIO . processRelease mf) mbRel

processRelease :: Manifest -> Release -> IO ()
processRelease Manifest {..} rel = do
	putStrLn ("processRelease: " ++ show rel)
	mbTors <- searchKickAss mManager (releaseName rel)
	case mbTors of
		Just tors -> do
			mapM_ (writeChan mChannel . ProcessTorrent rel)
			      (filter (\ info -> torrentInfoNormalized info == releaseName rel) tors)

		Nothing ->
			pure ()

processTorrentInfo :: Manifest -> Release -> TorrentInfo -> IO ()
processTorrentInfo Manifest {..} rel info = do
	putStrLn ("processTorrentInfo: " ++ show info)
	void $ runAction mDatabase $ do
		createTorrent info >>= addTorrent rel
