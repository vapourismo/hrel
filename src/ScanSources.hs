{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

import           Control.Monad

import           HRel.Sources
import           HRel.Torrents
import           HRel.Names

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

-- |
torrentSources :: [TorrentSource]
torrentSources =
	[RARBG "https://rarbg.to/rssdd_magnet.php?category=41",           -- TV
	 RARBG "https://rarbg.to/rssdd_magnet.php?category=48;44;45;42"]  -- Movies

-- |
torrentSourceWorker :: P.Connection -> Manager -> TorrentSource -> IO ()
torrentSourceWorker db mgr src = do
	mbTorrents <- processTorrentSource mgr src
	case mbTorrents of
		Just torrents ->
			forM_ torrents $ \ torrent@(Torrent title _) -> do
				upsertionResult <- runErrand db $ do
					tid <- insertTorrent torrent
					associateTags tid (parseTags title)

				case upsertionResult of
					Left err -> print err
					_        -> pure ()

				print title

		Nothing ->
			putStrLn ("Source " ++ show src ++ " errored")

-- |
main :: IO ()
main = do
	db <- P.connectdb "postgres://hrel@localhost/hrelhaskell"
	mgr <- newManager tlsManagerSettings
	forM_ torrentSources (torrentSourceWorker db mgr)
