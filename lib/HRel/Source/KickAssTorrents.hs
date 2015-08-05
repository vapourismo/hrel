{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.KickAssTorrents (
	kickAssSearchFilter,
	parseKickAssSearch,
	searchKickAss,
	fetchKickAssDump
) where

import           Control.Monad

import           Data.Char
import           Data.Maybe
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T

import           Network.URI

import           HRel.Data.Release
import           HRel.Data.Torrent

import           HRel.Markup
import           HRel.HTTP

-- | Filter for KickAss Torrents RSS search result.
kickAssSearchFilter :: NodeFilter B.ByteString [TorrentInfo]
kickAssSearchFilter =
	fmap concat $ relativeTag "rss" $ forTag "channel" $ foreachTag "item" $ do
		title   <- forTag "title"                 (T.strip . T.decodeUtf8 <$> text)
		size    <- forTag "torrent:contentLength" (T.strip . T.decodeUtf8 <$> text)
		magnet  <- forTag "torrent:magnetURI"     (T.strip . T.decodeUtf8 <$> text)
		torrent <- forTag "enclosure"             (T.strip . T.decodeUtf8 <$> attr "url")

		let uris = catMaybes [parseURI (T.unpack magnet), parseURI (cleanTorrentURL torrent)]
		guard (length uris > 0)

		let sizeNum =
			if T.all isDigit size then
				Just (read (T.unpack size))
			else
				Nothing

		pure (map (\ uri -> TorrentInfo title uri sizeNum) uris)
	where
		cleanTorrentURL =  T.unpack . fst . T.break (== '?')

-- | Parse the result to a KickAss Torrents RSS search.
parseKickAssSearch :: B.ByteString -> Maybe [TorrentInfo]
parseKickAssSearch contents =
	runNodeFilter kickAssSearchFilter (fromMarkup' contents)

-- | Use KickAss Torrents RSS search to find torrents matching a given release name.
searchKickAss :: Manager -> ReleaseName -> IO (Maybe [TorrentInfo])
searchKickAss mgr rel = do
	(>>= parseKickAssSearch) <$> download mgr requestURL
	where
		requestURL =
			"https://kat.cr/usearch/"
			++ escapeURIString isUnescapedInURI (T.unpack (getReleaseName rel))
			++ "/?rss=1"

-- | Download and parse a KickAss dump.
fetchKickAssDump :: Manager -> String -> IO (Maybe [TorrentInfo])
fetchKickAssDump mgr url =
	fmap processDump <$> downloadGZip mgr url
	where
		processDump contents =
			catMaybes (map processLine (BC.lines contents))

		processLine line = do
			(name, uriBS, sizeBS) <- case B.split 124 line of
				[_, name, _, _, uri, size, _, _, _, _, _, _] ->
					Just (name, uri, size)

				_ -> Nothing

			uri <- parseURI (BC.unpack uriBS)
			guard (BC.all isDigit sizeBS)

			pure (TorrentInfo (T.decodeUtf8 name) uri (Just (read (BC.unpack sizeBS))))
