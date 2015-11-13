{-# LANGUAGE OverloadedStrings #-}

module HRel.Sources.KickAssTorrents (
	parseKickAssTorrentFeed,
	parseKickAssTorrentDump
) where

import           Control.Monad

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString       as B
import qualified Data.Text.Encoding    as T
import qualified Data.Text             as T
import           Data.Maybe
import           Data.Char

import           Network.URI

import           HRel.Sources.ReleaseName
import           HRel.Sources.TorrentInfo
import           HRel.Sources.Markup

-- | Filter for KickAss Torrents RSS search result.
kickAssSearchFilter :: NodeFilter B.ByteString [TorrentInfo]
kickAssSearchFilter =
	fmap concat $ relativeTag "rss" $ forTag "channel" $ foreachTag "item" $ do
		title   <- forTag "title"                 (fromUTF8 <$> text)
		size    <- forTag "torrent:contentLength" (fromUTF8 <$> text)
		magnet  <- forTag "torrent:magnetURI"     (fromUTF8 <$> text)
		torrent <- forTag "enclosure"             (fromUTF8 <$> attr "url")

		let uris = catMaybes [parseURI (T.unpack magnet), parseURI (cleanTorrentURL torrent)]
		guard (length uris > 0)

		let sizeNum =
			if T.all isDigit size then
				Just (read (T.unpack size))
			else
				Nothing

		pure (map (\ uri -> TorrentInfo title (normalizeReleaseName title) uri sizeNum) uris)
	where
		cleanTorrentURL =  T.unpack . fst . T.break (== '?')
		fromUTF8 = T.strip . T.decodeUtf8

-- | Parse the result to a KickAss Torrents RSS search.
parseKickAssTorrentFeed :: B.ByteString -> Maybe [TorrentInfo]
parseKickAssTorrentFeed contents =
	runNodeFilter kickAssSearchFilter (fromMarkup' contents)

-- | Parse a KickAssTorrent dump.
parseKickAssTorrentDump :: B.ByteString -> [TorrentInfo]
parseKickAssTorrentDump contents =
	catMaybes (map processLine (BC.lines contents))
	where
		processLine line = do
			(name, uriBS, sizeBS) <- case B.split 124 line of
				[_, name, _, _, uri, size, _, _, _, _, _, _] ->
					Just (name, uri, size)

				_ -> Nothing

			uri <- parseURI (BC.unpack uriBS)
			guard (BC.all isDigit sizeBS)

			let nameText = T.strip (T.decodeUtf8 name)
			guard (not (T.null nameText))

			pure (TorrentInfo nameText
			                  (normalizeReleaseName nameText)
			                  uri
			                  (Just (read (BC.unpack sizeBS))))
