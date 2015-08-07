{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HRel.Processing (
	-- * Manifest
	Manifest (..),
	newManifest,

	-- * Workers
	WorkerCommand (..),
	queueCommand,
	spawnWorkers,
	spawnJobTimer
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

newManifest :: IO Manifest
newManifest =
	Manifest <$> connectDatabase <*> newTLSManager <*> newChan

data WorkerCommand
	= ProcessAllFeeds
	| ProcessFeed Feed
	| ProcessHourlyDump String
	| MatchTorrentsFor Feed
	| CleanDatabase

queueCommand :: Manifest -> WorkerCommand -> IO ()
queueCommand Manifest {..} =
	writeChan mChannel

spawnWorkers :: Manifest -> IO [ThreadId]
spawnWorkers Manifest {..} = do
	num <- getNumCapabilities
	replicateM (max 1 num) (forkIO worker)
	where
		worker = do
			db <- connectDatabase
			let mf = Manifest db mManager mChannel
			forever $ do
				msg <- readChan mChannel
				case msg of
					ProcessAllFeeds       -> processAllFeeds mf
					ProcessFeed feed      -> processFeed mf feed
					ProcessHourlyDump url -> processHourlyDump mf url
					MatchTorrentsFor feed -> matchTorrentsFor mf feed
					CleanDatabase         -> cleanDatabase mf

spawnJobTimer :: Manifest -> IO ()
spawnJobTimer mf = do
	repeatedTimer (queueCommand mf ProcessAllFeeds) (mDelay 20)
	repeatedTimer (queueCommand mf CleanDatabase) (hDelay 12)

	case confHourlyDump of
		Just dump -> void (repeatedTimer (queueCommand mf (ProcessHourlyDump dump)) (hDelay 1))
		Nothing   -> pure ()

processHourlyDump :: Manifest -> String -> IO ()
processHourlyDump Manifest {..} url = do
	mbTors <- fetchKickAssDump mManager url
	case mbTors of
		Just infos -> do
			mb <- runAction mDatabase (sum <$> mapM insertTorrents (makeChunks infos))
			case mb of
				Just num ->
					putStrLn ("processHourlyDump: Inserted " ++ show num ++ "/" ++ show (length infos))

				Nothing ->
					putStrLn ("processHourlyDump: Failed to insert all of " ++ show (length infos) ++ " received torrents")

			mbNum <- runAction mDatabase addMatchingTorrents
			case mbNum of
				Just num ->
					putStrLn ("processHourlyDump: Matched " ++ show num ++ " torrents")

				Nothing ->
					putStrLn "processHourlyDump: Could not connect any torrents"

		Nothing ->
			putStrLn "processHourlyDump: No dump available"
	where
		makeChunks [] = []
		makeChunks xs = take 16 xs : makeChunks (drop 16 xs)

processAllFeeds :: Manifest -> IO ()
processAllFeeds mf@(Manifest {..}) =
	runAction mDatabase findAllFeeds >>= maybe (pure ()) (mapM_ (queueCommand mf . ProcessFeed))

processFeed :: Manifest -> Feed -> IO ()
processFeed mf@(Manifest {..}) feed = do
	mbNames <- fetchAtomFeed mManager (show (feedURI feed))
	case mbNames of
		Just names -> void $ do
			mb <- runAction mDatabase (mapM createRelease names >>= addReleases feed)
			case mb of
				Just num ->
					putStrLn ("processFeed: Inserted " ++ show num ++ " for " ++ show (feedID feed))

				Nothing ->
					putStrLn ("processFeed: Failed to insert releases for " ++ show (feedID feed))

			matchTorrentsFor mf feed

		Nothing ->
			pure ()

matchTorrentsFor :: Manifest -> Feed -> IO ()
matchTorrentsFor Manifest {..} feed = do
	mbNum <- runAction mDatabase (addMatchingTorrentsFor feed)
	case mbNum of
		Just num ->
			putStrLn ("matchTorrentsFor: Matched " ++ show num ++ " torrents for " ++ show (feedID feed))

		Nothing ->
			putStrLn ("matchTorrentsFor: Could not connect any torrents for " ++ show (feedID feed))

cleanDatabase :: Manifest -> IO ()
cleanDatabase Manifest {..} = do
	mbNum <- runAction mDatabase removeUnusedTorrents
	case mbNum of
		Just num ->
			putStrLn ("cleanDatabase: Removed " ++ show num ++ " torrents")

		Nothing ->
			putStrLn "cleanDatabase: Could not remove unused torrents"
