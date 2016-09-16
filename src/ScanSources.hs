{-# LANGUAGE OverloadedStrings, TemplateHaskell	, QuasiQuotes #-}

import           Control.Monad
import           Control.Monad.Except
import           Control.Exception
import           Control.Concurrent

import           HRel.Models
import           HRel.Sources

import           Data.Maybe

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

-- |
torrentSources :: [TorrentSource]
torrentSources =
	[PirateBay "https://thepiratebay.org/rss/top100/0",
	 PirateBay "https://thepiratebay.org/rss/top100/201",
	 PirateBay "https://thepiratebay.org/rss/top100/205",
	 PirateBay "https://thepiratebay.org/rss/top100/207",
	 PirateBay "https://thepiratebay.org/rss/top100/208"]

-- |
torrentSourceWorker :: P.Connection -> Manager -> TorrentSource -> IO ()
torrentSourceWorker db mgr src = do
	mbTorrents <- processTorrentSource mgr src
	case mbTorrents of
		Just torrents ->
			mapM_ print torrents

		Nothing ->
			putStrLn ("Source " ++ show src ++ " errored")

-- |
forkWorker :: IO () -> IO (MVar ())
forkWorker action = do
	mvar <- newEmptyMVar
	forkFinally action $ \ result -> do
		case result of
			Left (SomeException e) ->
				putStrLn ("Exception in worker: " ++ show e)

			Right _ -> pure ()

		putMVar mvar ()

	pure mvar

-- |
main :: IO ()
main = do
	db <- P.connectdb "postgres://hrel@localhost/hrel"
	mgr <- newManager tlsManagerSettings

	mvars <- forM torrentSources $ \ src ->
		forkWorker (torrentSourceWorker db mgr src)

	mapM_ takeMVar mvars
