{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans

import           Data.Aeson (object, (.=))
import qualified Data.Text as T

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           Network.HTTP.Types
import           Web.Scotty

import           HRel.Names
import           HRel.Torrents

search :: T.Text -> Errand [Torrent]
search =
	searchForTorrents . parseTags

performSearch :: P.Connection -> ActionM ()
performSearch db = do
	searchTerm <- param "q"
	result <- liftIO (runErrand db (search searchTerm))

	case result of
		Left err -> do
			liftIO (print err)
			status internalServerError500
			text "Search failed"

		Right torrents ->
			json (map transformResult torrents)

	where
		transformResult (Torrent title uri) =
			object ["title" .= title,
			        "uri"   .= uri]

main :: IO ()
main = do
	db <- P.connectdb "user=hrel dbname=hrel"

	scotty 3401 $ do
		get "/search" (performSearch db)
