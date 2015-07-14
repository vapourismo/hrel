--{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.KickAssTorrents (
	kickAssSearch
) where

---- | Download data dump and parse its contents.
--fetchFromDump :: String -> Manager -> IO [Torrent]
--fetchFromDump url mgr = do
--	req <- parseUrl url
--	withResponse req mgr $ \ res ->
--		case responseStatus res of
--			Status 200 _ ->
--				fmap (pickTorrents . decode . B.concat)
--				     (brConsume (responseBody res))

--			Status _   _ ->
--				pure []
--	where
--		decode = T.decodeUtf8 . L.toStrict . Z.decompress . L.fromStrict

--		toTorrent line =
--			case T.split (== '|') line of
--				[_, name, _, _, downloadURI, size,_, _, _, _, _] -> do
--				 	uri <- parseURI (T.unpack downloadURI)
--					pure (Torrent (makeRelease (T.copy name)) [uri] (Just (read (T.unpack size))))

--				_ -> Nothing

--		pickTorrents = catMaybes . map toTorrent . T.lines

import           Control.Monad

import           Data.Char
import           Data.Maybe
import           Data.Conduit
import qualified Data.Conduit.List         as C
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T

import           Network.URI

import           HRel.Conduit
import           HRel.Markup
import           HRel.Release
import           HRel.Torrent

-- | Search for "Torrent"s which match the given "Release".
kickAssSearch :: HRelConduit Release Torrent
kickAssSearch =
	conduit
	where
		conduit =
			await >>= maybe (pure ()) (\ rel -> performSearch rel >> conduit)

		performSearch rel =
			request ("https://kat.cr/usearch/"
			         ++ escapeURIString isUnescapedInURI (T.unpack (fromRelease rel))
			         ++ "/?rss=1")
				=$= fetch
				=$= C.map T.decodeUtf8
				=$= markup (xmlFilter rel)
				=$= C.concat

		xmlFilter cmpRelease =
			relativeTag "rss" $ forTag "channel" $ foreachTag "item" $ do
				title   <- forTag "title"                 (T.strip <$> text)
				size    <- forTag "torrent:contentLength" (T.strip <$> text)
				magnet  <- forTag "torrent:magnetURI"     (T.strip <$> text)
				torrent <- forTag "enclosure"             (T.strip <$> attr "url")

				let release = makeRelease (T.copy title)
				guard (release == cmpRelease)

				let uris = catMaybes [makeURI magnet,
				                      makeURI (cleanTorrentURI torrent)]
				guard (length uris > 0)

				let sizeNum =
					if T.all isDigit size then
						Just (read (T.unpack size))
					else
						Nothing

				pure (Torrent release uris sizeNum)

		cleanTorrentURI = fst . T.break (== '?')

		makeURI = parseURI . T.unpack


---- | Search for "Torrent"s which match the given "Release".
--kickAssReleaseSearch :: (MonadThrow m, MonadIO m) => Manager -> Conduit Release m Torrent
--kickAssReleaseSearch mgr = do
--	r <- await
--	case r of
--		Nothing  ->
--			pure ()

--		Just rel -> do
--			request (makeURL rel)
--				=$= fetch mgr
--				=$= C.map (T.decodeUtf8 . BL.toStrict)
--				=$= markup rssFilter
--				=$= C.concat
--				=$= C.filter (\ tor -> torrentRelease tor == rel)
--			kickAssReleaseSearch mgr

--	where
--		makeURL rel =
--			"https://kat.cr/usearch/"
--			++ escapeURIString isUnescapedInURI (T.unpack (toText rel))
--			++ "/?rss=1"

--		rssFilter =
--			relativeTag "rss" $ forTag "channel" $
--				foreachTag "item" $ do
--					title <- forTag "title" text
--					size <- forTag "torrent:contentLength" text
--					magnetURIString <- forTag "torrent:magnetURI" text
--					torrentURIString <- forTag "enclosure" (attr "url")

--					magnetURI <- toURI (T.strip magnetURIString)
--					torrentURI <- toURI (cleanTorrentURI torrentURIString)

--					pure (Torrent (makeRelease (T.copy title))
--					              [magnetURI, torrentURI]
--					              (Just (read (T.unpack (T.strip size)))))

--		cleanTorrentURI = fst . T.break (== '?') . T.strip

--		toURI = MaybeT . pure . parseURI . T.unpack
