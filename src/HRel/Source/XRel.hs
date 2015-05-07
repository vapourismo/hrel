module HRel.Source.XRel (
) where

import Control.Monad.Trans.Maybe

import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.URI

import HRel.Source
import HRel.Markup


-- |
--fetchFromAtom :: String -> Manager -> IO [Release]
--fetchFromAtom url mgr = do
--	req <- parseUrl url
--	withResponse req mgr $ \ res ->
--		case responseStatus res of
--			Status 200 _ ->
--				fmap (maybe [] id . runFilter . fromMarkup' . decode)
--				     (brConsume (responseBody res))

--			Status _   _ ->
--				return []

--	where
--		decode = T.decodeUtf8 . B.concat

--		toURI = MaybeT . return . parseURI . T.unpack

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

--		runFilter = runNodeFilter rssFilter
