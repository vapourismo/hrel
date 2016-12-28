{-# LANGUAGE OverloadedStrings #-}

module HRel.Sources (
	pirateBaySource,

	TorrentSource (..),
	processTorrentSource
) where

import           HRel.NodeFilter
import           HRel.Torrents
import           HRel.Network

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Network.HTTP.Client

-- | Pirate Bay source
pirateBaySource :: NodeFilter [Torrent]
pirateBaySource =
	forRelativeTag "rss" $ "channel" $/ "item" $//
		buildTorrent <$> ("title" $/ text)
		             <*> ("torrent" $/ "magnetURI" $/ text)
	where
		buildTorrent title uri =
			Torrent (T.strip title) (T.strip uri)

-- | RARBG source
rarbgSource :: NodeFilter [Torrent]
rarbgSource =
	"channel" $/ "item" $//
		buildTorrent <$> ("title" $/ text)
		             <*> ("link" $/ text)
	where
		buildTorrent title uri =
			Torrent (T.strip title) (T.strip uri)

-- | A source which aggregates torrents
data TorrentSource
	= PirateBay String
	| RARBG String
	deriving (Show, Eq, Ord)

-- | Process the torrent source in order to retrieve the torrents.
processTorrentSource :: Manager -> TorrentSource -> IO (Maybe [Torrent])
processTorrentSource mgr src = do
	case src of
		PirateBay url -> downloadMarkup mgr url pirateBaySource
		RARBG url     -> downloadMarkup mgr url rarbgSource
