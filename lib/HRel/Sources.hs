{-# LANGUAGE OverloadedStrings #-}

module HRel.Sources (
	pirateBaySource,

	TorrentSource (..),
	processTorrentSource
) where

import           HRel.NodeFilter
import           HRel.Torrents
import           HRel.Network
import           HRel.Markup

import qualified Data.Text          as T

import           Network.HTTP.Client

-- | Pirate Bay source
pirateBaySource :: NodeFilter T.Text [Torrent]
pirateBaySource =
	forRelativeTag "rss" $ "channel" $/ "item" $//
		buildTorrent <$> ("title" $/ text)
		             <*> ("torrent" $/ "magnetURI" $/ text)
	where
		buildTorrent title uri =
			Torrent (T.strip title) (T.strip uri)

-- | RARBG source
rarbgSource :: NodeFilter T.Text [Torrent]
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

-- | Download a feed at a given URL in order to filter its results.
downloadAndFilter :: Manager -> String -> NodeFilter T.Text a -> IO (Maybe a)
downloadAndFilter mgr url flt = do
	mbContents <- download mgr url
	pure (mbContents >>= parseMarkup_ >>= flip runNodeFilter flt)

-- | Process the torrent source in order to retrieve the torrents.
processTorrentSource :: Manager -> TorrentSource -> IO (Maybe [Torrent])
processTorrentSource mgr src = do
	case src of
		PirateBay url -> downloadAndFilter mgr url pirateBaySource
		RARBG url     -> downloadAndFilter mgr url rarbgSource
