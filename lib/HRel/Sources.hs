{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module HRel.Sources (
	TorrentSource (..),

	pirateBaySource,
	rarbgSource,

	SourceError (..),

	TorrentProducer,
	torrentSource,

	TorrentConduit,
	fetchTorrents
) where

import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.Resource

import           Data.Conduit
import           Data.Conduit.Text

import qualified Data.Text as T

import           Network.HTTP.Client

import           HRel.NodeFilter
import           HRel.Network
import           HRel.Markup
import           HRel.Torrents
import           HRel.Monad

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

-- | An error that might occur during the processing of a source
data SourceError
	= XmlError XmlError
	| HttpError HttpError
	| NodeFilterError NodeFilterError
	| InvalidUrl String
	deriving (Show)

-- | Torrent producer
type TorrentProducer m o = forall i. HRelT SourceError (ConduitM i o) m ()

-- | Process the given torrent source.
torrentSource :: (MonadCatch m, MonadResource m) => Manager -> TorrentSource -> TorrentProducer m [Torrent]
torrentSource mgr src = do
	req <- lift $
		case parseRequest url of
			Just x  -> pure x
			Nothing -> throwError (InvalidUrl url)

	withHRelT HttpError (httpRequest mgr req)
		=$= decode utf8
		=$= withHRelT XmlError processXML
		=$= withHRelT NodeFilterError (filterNodes nf)
	where
		(url, nf) =
			case src of
				PirateBay url -> (url, pirateBaySource)
				RARBG url     -> (url, rarbgSource)

-- | Source transformer, torrent producer
type TorrentConduit i m o = HRelT SourceError (ConduitM i o) m ()

-- | Fetch torrents from the given sources
fetchTorrents :: (MonadCatch m, MonadResource m) => Manager -> TorrentConduit TorrentSource m [Torrent]
fetchTorrents mgr =
	awaitForever (torrentSource mgr)
