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
torrentSources = [PirateBay "https://thepiratebay.org/rss/top100/0"]

-- |
insertTorrents :: P.Connection -> [Torrent] -> IO ()
insertTorrents db torrents = do
	res <- runErrand db (mapM insertTorrent torrents)
	case res of
		Left err ->
			putStrLn ("Error during Errand: " ++ show err)

		Right refs ->
			putStrLn ("Inserted " ++ show (length (catMaybes refs)) ++ " torrents")

	where
		insertTorrent tor =
			catchError (Just <$> insert tor) $ \ err ->
				case err of
					ExecError _ UniqueViolation _ _ _ ->
						pure Nothing

					_ -> throwError err

-- |
torrentSourceWorker :: P.Connection -> Manager -> TorrentSource -> IO ()
torrentSourceWorker db mgr src = do
	mbTorrents <- processTorrentSource mgr src
	case mbTorrents of
		Just torrents ->
			insertTorrents db torrents

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
