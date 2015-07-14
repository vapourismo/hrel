{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.KickAssTorrents (
	kickAssSearch
) where

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
