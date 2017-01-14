{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module HRel.Sources (
	pirateBaySource,
	rarbgSource,

	TorrentSource (..),
	torrentSource
) where

import           Control.Monad.Trans.Resource

import           Data.Conduit
import           Data.Conduit.Text

import qualified Data.Text as T

import           Network.HTTP.Client

import           HRel.NodeFilter
import           HRel.Network
import           HRel.Markup
import           HRel.Torrents

-- | Pirate Bay source
pirateBaySource :: NodeFilter [Torrent]
pirateBaySource =
	"channel" $/ "item" $//
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

-- | Process the given torrent source.
torrentSource :: (MonadResource m) => Manager -> TorrentSource -> Producer m [Torrent]
torrentSource mgr src =
	httpRequest mgr url
		=$= decode utf8
		=$= processXML
		=$= filterNodes nf
	where
		(url, nf) =
			case src of
				PirateBay url -> (url, pirateBaySource)
				RARBG url     -> (url, rarbgSource)
