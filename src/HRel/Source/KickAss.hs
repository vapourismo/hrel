{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.KickAss (
	kickAssHourly,
	kickAssDaily,
	kickAssSearch
) where

import Control.Monad.Trans.Maybe

import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Codec.Compression.GZip as Z

import Network.HTTP.Client
import Network.HTTP.Types

import Network.URI

import HRel.Source
import HRel.Markup

-- | Download data dump and parse its contents.
fetchFromDump :: String -> Manager -> IO [Torrent]
fetchFromDump url mgr = do
	req <- parseUrl url
	withResponse req mgr $ \ res ->
		case responseStatus res of
			Status 200 _ ->
				fmap (pickTorrents . decode . B.concat)
				     (brConsume (responseBody res))

			Status _   _ ->
				return []
	where
		decode = T.decodeUtf8 . L.toStrict . Z.decompress . L.fromStrict

		toTorrent line =
			case T.split (== '|') line of
				[_, name, _, _, downloadURI, size,_, _, _, _, _] -> do
				 	uri <- parseURI (T.unpack downloadURI)
					pure (Torrent (T.copy name) [uri] (Just (read (T.unpack size))))

				_ -> Nothing

		pickTorrents = catMaybes . map toTorrent . T.lines

-- | Fetch results from RSS feed.
fetchFromRSS :: String -> Manager -> IO [Torrent]
fetchFromRSS url mgr = do
	req <- parseUrl url
	withResponse req mgr $ \ res ->
		case responseStatus res of
			Status 200 _ -> do
				fmap (runFilter . fromMarkup' . decode) (brConsume (responseBody res))
					>>= print
				return []

			Status _   _ ->
				return []
		where
			decode = T.decodeUtf8 . B.concat

			toURI = MaybeT . return . parseURI . T.unpack

			rssFilter =
				relativeTag "rss" $ forTag "channel" $
					foreachTag "item" $ do
						title <- forTag "title" text
						size <- forTag "torrent:contentLength" text
						magnetURI <- forTag "torrent:magnetURI" text
						             >>= toURI . T.strip
						torrentURI <- forTag "enclosure" (attr "url")
						              >>= toURI . fst . T.break (== '?') . T.strip
						return (Torrent (T.strip title)
						                [magnetURI, torrentURI]
						                (Just (read (T.unpack (T.strip size)))))

			runFilter = runNodeFilter rssFilter

-- | Hourly dump from 'kickass.to'
kickAssHourly :: Aggregator
kickAssHourly =	Aggregator (fetchFromDump "https://kickass.to/hourlydump.txt.gz")

-- | Daily dump from 'kickass.to'
kickAssDaily :: Aggregator
kickAssDaily = Aggregator (fetchFromDump "https://kickass.to/dailydump.txt.gz")

-- | Search on 'kickass.to'
kickAssSearch :: String -> Aggregator
kickAssSearch term =
	Aggregator (fetchFromRSS ("https://kickass.to/usearch/"
	                          ++ escapeURIString isUnescapedInURI term
	                          ++ "/?rss=1"))
