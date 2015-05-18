{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.KickAss (
	--kickAssHourly,
	--kickAssDaily,
	--kickAssSearch,
	--kickAssReleaseSearch,
) where

----import Control.Monad.Trans
----import Control.Monad.Reader
--import Control.Monad.Trans.Maybe

--import Data.Maybe

----import Data.Conduit

--import qualified Data.ByteString as B
--import qualified Data.ByteString.Lazy as L

--import qualified Data.Text as T
--import qualified Data.Text.Encoding as T

--import qualified Codec.Compression.GZip as Z

--import Network.URI
--import Network.HTTP.Types

--import HRel.Source
--import HRel.Release
--import HRel.Markup

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
--				return []
--	where
--		decode = T.decodeUtf8 . L.toStrict . Z.decompress . L.fromStrict

--		toTorrent line =
--			case T.split (== '|') line of
--				[_, name, _, _, downloadURI, size,_, _, _, _, _] -> do
--				 	uri <- parseURI (T.unpack downloadURI)
--					pure (Torrent (makeRelease (T.copy name)) [uri] (Just (read (T.unpack size))))

--				_ -> Nothing

--		pickTorrents = catMaybes . map toTorrent . T.lines

---- | Fetch results from RSS feed.
--fetchFromRSS :: String -> Manager -> IO [Torrent]
--fetchFromRSS url mgr = do
--	req <- parseUrl url
--	withTextResponse req mgr (pure . maybe [] id . runNodeFilter rssFilter . fromMarkup')
--	where
--		rssFilter =
--			relativeTag "rss" $ forTag "channel" $
--				foreachTag "item" $ do
--					title <- forTag "title" text
--					size <- forTag "torrent:contentLength" text
--					magnetURI <- forTag "torrent:magnetURI" text
--					             >>= toURI . T.strip
--					torrentURI <- forTag "enclosure" (attr "url")
--					              >>= toURI . fst . T.break (== '?') . T.strip
--					return (Torrent (makeRelease (T.copy title))
--					                [magnetURI, torrentURI]
--					                (Just (read (T.unpack (T.strip size)))))

--		toURI = MaybeT . return . parseURI . T.unpack

------ | Hourly dump from 'kickass.to'
----kickAssHourly_ :: Producer (ReaderT Manager IO) Torrent
----kickAssHourly_ = do
----	lift ask
----		>>= liftIO . fetchFromDump "https://kickass.to/hourlydump.txt.gz"
----		>>= mapM_ yield

---- | Hourly dump from 'kickass.to'
--kickAssHourly :: Aggregator Torrent
--kickAssHourly =	Aggregator (fetchFromDump "https://kickass.to/hourlydump.txt.gz")

------ | Daily dump from 'kickass.to'
----kickAssDaily_ :: Producer (ReaderT Manager IO) Torrent
----kickAssDaily_ =
----	lift ask
----		>>= liftIO . fetchFromDump "https://kickass.to/dailydump.txt.gz"
----		>>= mapM_ yield

---- | Daily dump from 'kickass.to'
--kickAssDaily :: Aggregator Torrent
--kickAssDaily = Aggregator (fetchFromDump "https://kickass.to/dailydump.txt.gz")

---- | Search on 'kickass.to'
--kickAssSearch :: String -> Aggregator Torrent
--kickAssSearch term =
--	Aggregator (fetchFromRSS ("https://kickass.to/usearch/"
--	                          ++ escapeURIString isUnescapedInURI term
--	                          ++ "/?rss=1"))

------ | Search for a release on 'kickass.to'
----kickAssReleaseSearch_ :: Conduit Release (ReaderT Manager IO) Torrent
----kickAssReleaseSearch_ =
----	await >>= maybe (pure ()) continue
----	where
----		continue :: Release -> Conduit Release (ReaderT Manager IO) Torrent
----		continue rel =
----			lift ask >>= liftIO . fetchFromRSS ("https://kickass.to/usearch/"
----			                                    ++ escapeURIString isUnescapedInURI (T.unpack (toText rel))
----			                                    ++ "/?rss=1")
----			         >>= mapM_ (\ tor -> when (torrentRelease tor == rel) (yield tor))

---- | Search for a release on 'kickass.to'
--kickAssReleaseSearch :: Release -> Aggregator Torrent
--kickAssReleaseSearch release = do
--	torrent <- kickAssSearch (T.unpack (toText release))
--	if torrentRelease torrent == release then
--		pure torrent
--	else
--		mempty
