{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.KickAssTorrents (
	kickAssSearch
) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Catch

import           Data.Char
import           Data.Maybe
import           Data.Conduit
import qualified Data.Conduit.List  as C
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Network.URI
import           Network.HTTP.Client

import           HRel.Conduit
import           HRel.Markup
import           HRel.Data.Release
import           HRel.Data.Torrent

-- | Search for torrents which match the given release name.
kickAssSearch :: (MonadIO m, MonadThrow m) => Manager -> Conduit ReleaseName m TorrentInfo
kickAssSearch mgr =
	await >>= maybe (pure ()) (\ rel -> performSearch rel >> kickAssSearch mgr)
	where
		performSearch rel =
			request ("https://kat.cr/usearch/"
			         ++ escapeURIString isUnescapedInURI (T.unpack (getReleaseName rel))
			         ++ "/?rss=1")
				=$= fetch mgr
				=$= C.map T.decodeUtf8
				=$= markup (xmlFilter rel)
				=$= C.concat

		xmlFilter cmpRelease =
			fmap concat $ relativeTag "rss" $ forTag "channel" $ foreachTag "item" $ do
				title   <- forTag "title"                 (T.strip <$> text)
				size    <- forTag "torrent:contentLength" (T.strip <$> text)
				magnet  <- forTag "torrent:magnetURI"     (T.strip <$> text)
				torrent <- forTag "enclosure"             (T.strip <$> attr "url")

				let release = normalizeReleaseName (T.copy title)
				guard (release == cmpRelease)

				let uris = catMaybes [makeURI magnet,
				                      makeURI (cleanTorrentURI torrent)]
				guard (length uris > 0)

				let sizeNum =
					if T.all isDigit size then
						Just (read (T.unpack size))
					else
						Nothing

				pure (map (\ uri -> TorrentInfo release uri sizeNum) uris)

		cleanTorrentURI = fst . T.break (== '?')

		makeURI = parseURI . T.unpack
