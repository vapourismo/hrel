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
	queueProcessAllFeeds,
	queueProcessFeed,
	processHourlyDump
) where

import Control.Monad

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
	= ProcessAllFeeds
	| ProcessFeed Feed

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
					ProcessFeed feed -> processFeed mf feed
					ProcessAllFeeds  -> processAllFeeds mf

spawnJobTimer :: Manifest -> IO ()
spawnJobTimer mf = do
	repeatedTimer (queueProcessAllFeeds mf) (mDelay 20)

	case confHourlyDump of
		Just dump -> void (repeatedTimer (processHourlyDump mf dump) (hDelay 1))
		Nothing   -> pure ()

processHourlyDump :: Manifest -> String -> IO ()
processHourlyDump Manifest {..} url = do
	mbTors <- fetchKickAssDump mManager url
	case mbTors of
		Just infos -> void $ do
			mb <- runAction mDatabase (sum <$> mapM insertTorrents (makeChunks infos))
			case mb of
				Just num ->
					putStrLn ("processHourlyDump: Inserted " ++ show num ++ "/" ++ show (length infos))

				Nothing ->
					putStrLn ("processHourlyDump: Failed to insert all of " ++ show (length infos) ++ " received torrents")

			runAction mDatabase addMatchingTorrents

		Nothing ->
			putStrLn "processHourlyDump: No dump available"
	where
		makeChunks [] = []
		makeChunks xs = take 16 xs : makeChunks (drop 16 xs)

queueProcessAllFeeds :: Manifest -> IO ()
queueProcessAllFeeds Manifest {..} =
	writeChan mChannel ProcessAllFeeds

processAllFeeds :: Manifest -> IO ()
processAllFeeds mf@(Manifest {..}) =
	runAction mDatabase findAllFeeds >>= maybe (pure ()) (mapM_ (queueProcessFeed mf))

queueProcessFeed :: Manifest -> Feed -> IO ()
queueProcessFeed Manifest {..} feed =
	writeChan mChannel (ProcessFeed feed)

processFeed :: Manifest -> Feed -> IO ()
processFeed Manifest {..} feed = do
	mbNames <- fetchAtomFeed mManager (show (feedURI feed))
	case mbNames of
		Just names -> void $ do
			runAction mDatabase (mapM createRelease names >>= addReleases feed)
			runAction mDatabase (addMatchingTorrentsFor feed)

		Nothing ->
			pure ()
