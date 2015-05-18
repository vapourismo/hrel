{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.KickAss (
	kickAssReleaseSearch
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

import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString.Lazy as BL

import Data.Conduit
import qualified Data.Conduit.List as C

import Network.URI

import HRel.Markup
import HRel.Conduit
import HRel.Release
import HRel.Torrent

-- | Search for "Torrent"s that match the given "Release".
kickAssReleaseSearch :: (MonadThrow m, MonadIO m) => Conduit Release (FetchT m) Torrent
kickAssReleaseSearch = do
	r <- await
	case r of
		Nothing  ->
			pure ()

		Just rel ->
			request (makeURL rel)
				=$= fetch
				=$= C.map (T.decodeUtf8 . BL.toStrict)
				=$= markup rssFilter
				=$= C.concat
				=$= C.filter (\ tor -> torrentRelease tor == rel)

	where
		makeURL rel =
			"https://kickass.to/usearch/"
			++ escapeURIString isUnescapedInURI (T.unpack (toText rel))
			++ "/?rss=1"

		rssFilter =
			relativeTag "rss" $ forTag "channel" $
				foreachTag "item" $ do
					title <- forTag "title" text
					size <- forTag "torrent:contentLength" text
					magnetURIString <- forTag "torrent:magnetURI" text
					torrentURIString <- forTag "enclosure" (attr "url")

					magnetURI <- toURI (T.strip magnetURIString)
					torrentURI <- toURI (removeQuery torrentURIString)

					pure (Torrent (makeRelease (T.copy title))
					              [magnetURI, torrentURI]
					              (Just (read (T.unpack (T.strip size)))))

		removeQuery = fst . T.break (== '?') . T.strip

		toURI = MaybeT . pure . parseURI . T.unpack
