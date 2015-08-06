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
	processAllFeeds,
) where

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.Suspend
import Control.Concurrent.Timer

import HRel.Database
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

spawnJobTimer :: Manifest -> IO TimerIO
spawnJobTimer mf =
	repeatedTimer (processAllFeeds mf) (mDelay 20)

processAllFeeds :: Manifest -> IO ()
processAllFeeds Manifest {..} =
	runAction mDatabase findAllFeeds >>= mapM_ (writeChan mChannel . ProcessFeed)

queueProcessFeed :: Manifest -> Feed -> IO ()
queueProcessFeed Manifest {..} feed =
	writeChan mChannel (ProcessFeed feed)

processFeed :: Manifest -> Feed -> IO ()
processFeed Manifest {..} feed = do
	putStrLn ("processFeed: " ++ show feed)
	mbRels <- fetchAtomFeed mManager (show (feedURI feed))
	maybe (pure ()) (mapM_ (writeChan mChannel . ProcessFeedEntry feed)) mbRels

processFeedEntry :: Manifest -> Feed -> ReleaseName -> IO ()
processFeedEntry mf@(Manifest {..}) feed name = do
	putStrLn ("processFeedEntry: " ++ show name)
	runAction mDatabase $ do
		mbRel <- createRelease name
		case mbRel of
			Just rel -> do
				addRelease feed rel
				liftIO (processRelease mf rel)

			Nothing ->
				pure ()

processRelease :: Manifest -> Release -> IO ()
processRelease Manifest {..} rel = do
	putStrLn ("processRelease: " ++ show rel)
	mbTors <- searchKickAss mManager (releaseName rel)
	case mbTors of
		Just tors -> do
			mapM_ (writeChan mChannel . ProcessTorrent rel)
			      (filter (\ info -> normalizeReleaseName (torrentInfoName info) == releaseName rel)
			              tors)

		Nothing ->
			pure ()

processTorrentInfo :: Manifest -> Release -> TorrentInfo -> IO ()
processTorrentInfo Manifest {..} rel info = do
	putStrLn ("processTorrentInfo: " ++ show info)
	runAction mDatabase $ do
		createTorrent info >>= maybe (pure ()) (addTorrent rel)
