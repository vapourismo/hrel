{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent.MVar
-- import           Control.Monad.Trans

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           Happstack.Server

import           HRel.Names
import           HRel.Torrents

httpConf :: Conf
httpConf =
	nullConf {
		port = 3401
	}

defaultRoute :: ServerPartT IO Response
defaultRoute =
	badRequest (toResponse ("Bad request" :: String))

requestBody :: ServerPartT IO BL.ByteString
requestBody =
	unBody <$> (askRq >>= liftIO . readMVar . rqBody)

decodeQueryString :: BL.ByteString -> T.Text
decodeQueryString body =
	either (const T.empty) id (T.decodeUtf8' (BL.toStrict body))

search :: T.Text -> Errand [(Torrent, Word)]
search =
	searchForTorrents . parseTags

searchRoute :: P.Connection -> ServerPartT IO Response
searchRoute db =
	dir "search" $ do
		body <- requestBody
		result <- liftIO (runErrand db (search (decodeQueryString body)))

		case result of
			Left err -> do
				liftIO (print err)
				internalServerError (toResponse ("Search failed" :: String))

			Right torrents ->
				ok (toResponse (encode (map transformResult torrents)))

	where
		transformResult (Torrent title uri, score) =
			object ["title" .= title,
			        "uri"   .= uri,
			        "score" .= score]

main :: IO ()
main = do
	db <- P.connectdb "postgres://hrel@localhost/hrel"

	simpleHTTP httpConf (msum [searchRoute db,
	                           defaultRoute])
