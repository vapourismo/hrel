{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Applicative
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

search :: T.Text -> Errand [Torrent]
search =
	searchForTorrents . parseTags

searchRoutePost :: P.Connection -> ServerPartT IO Response
searchRoutePost db = do
	method POST
	requestBody >>= searchRoute db

searchRouteGet :: P.Connection -> ServerPartT IO Response
searchRouteGet db = do
	method GET
	decodeBody (defaultBodyPolicy "/tmp" 1024 1024 1024)
	lookBS "q" >>= searchRoute db

searchRoute :: P.Connection -> BL.ByteString -> ServerPartT IO Response
searchRoute db searchTerm = do
	result <- liftIO (runErrand db (search (decodeQueryString searchTerm)))
	case result of
		Left err -> do
			liftIO (print err)
			internalServerError (toResponse ("Search failed" :: String))

		Right torrents -> do
			setHeaderM "Content-Type" "application/json"
			ok (toResponse (encode (map transformResult torrents)))

	where
		transformResult (Torrent title uri) =
			object ["title" .= title,
			        "uri"   .= uri]

main :: IO ()
main = do
	db <- P.connectdb "user=hrel dbname=hrel"

	simpleHTTP httpConf (msum [dir "search" (searchRoutePost db <|> searchRouteGet db),
	                           defaultRoute])
