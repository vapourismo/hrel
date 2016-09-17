{-# LANGUAGE OverloadedStrings, TemplateHaskell	, QuasiQuotes #-}

import           Control.Monad

import           HRel.Sources
import           HRel.Models
import           HRel.Names

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

-- |
torrentSources :: [TorrentSource]
torrentSources =
	[PirateBay "https://thepiratebay.org/rss/top100/0",
	 PirateBay "https://thepiratebay.org/rss/top100/201",
	 PirateBay "https://thepiratebay.org/rss/top100/205",
	 PirateBay "https://thepiratebay.org/rss/top100/207",
	 PirateBay "https://thepiratebay.org/rss/top100/208"]

-- |
torrentSourceWorker :: Manager -> TorrentSource -> IO ()
torrentSourceWorker mgr src = do
	mbTorrents <- processTorrentSource mgr src
	case mbTorrents of
		Just torrents ->
			forM_ torrents $ \ (Torrent title _) -> do
				print title

				let (nameTags, qualifiers) = parseNameTags title

				putStr "\t"
				print nameTags

				putStr "\t"
				print qualifiers

		Nothing ->
			putStrLn ("Source " ++ show src ++ " errored")

-- |
main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings
	forM_ torrentSources (torrentSourceWorker mgr)
