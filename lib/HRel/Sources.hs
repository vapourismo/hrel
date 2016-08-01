{-# LANGUAGE OverloadedStrings #-}

module HRel.Sources (
	pirateBaySource,

	TorrentSource (..),
	processTorrentSource
) where

import           HRel.NodeFilter
import           HRel.Models
import           HRel.Network
import           HRel.Markup

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Network.HTTP.Client

-- | Pirate Bay RSS source.
pirateBaySource :: NodeFilter B.ByteString [Torrent]
pirateBaySource =
	forRelativeTag "rss" $ "channel" $/ "item" $//
		buildTorrent <$> ("title" $/ text)
		             <*> ("torrent" $/ "magnetURI" $/ text)
	where
		buildTorrent title uri =
			Torrent (T.strip (T.decodeUtf8 title)) (T.strip (T.decodeUtf8 uri))

-- |
data TorrentSource
	= PirateBay String
	deriving (Show, Eq, Ord)

-- |
processTorrentSource :: Manager -> TorrentSource -> IO (Maybe [Torrent])
processTorrentSource mgr src = do
	case src of
		PirateBay url -> do
			mbContents <- download mgr url
			pure (mbContents >>= parseMarkup_ >>= flip runNodeFilter pirateBaySource)
