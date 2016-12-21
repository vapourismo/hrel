{-# LANGUAGE OverloadedStrings, TemplateHaskell	, QuasiQuotes #-}

import           Control.Monad

import           HRel.Sources
import           HRel.Models
import           HRel.Names

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

-- |
torrentSources :: [TorrentSource]
torrentSources =
	[RARBG "https://rarbg.to/rssdd.php?category=43",         -- TV
	 RARBG "https://rarbg.to/rssdd.php?category=48;44;45;42"]  -- Movies

-- |
torrentSourceWorker :: P.Connection -> Manager -> TorrentSource -> IO ()
torrentSourceWorker db mgr src = do
	mbTorrents <- processTorrentSource mgr src
	case mbTorrents of
		Just torrents ->
			forM_ torrents $ \ torrent@(Torrent title _) -> do
				let (nameTags, qualifiers) = parseNameTags title

				upsertionResult <- runErrand db $ do
					tid <- insertTorrent torrent
					associateTags tid (nameTags ++ qualifiers)

				case upsertionResult of
					Left err -> print err
					_        -> pure ()

				print title

				putStr "\t"
				print nameTags

				putStr "\t"
				print qualifiers

		Nothing ->
			putStrLn ("Source " ++ show src ++ " errored")

-- |
main :: IO ()
main = do
	db <- P.connectdb "postgres://hrel@localhost/hrelhaskell"
	mgr <- newManager tlsManagerSettings
	forM_ torrentSources (torrentSourceWorker db mgr)
