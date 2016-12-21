{-# LANGUAGE OverloadedStrings, DeriveGeneric, QuasiQuotes #-}

module HRel.Models (
	Release (..),

	Torrent (..),
	insertTorrent,
	searchForTorrents,

	associateTags
) where

import           GHC.Generics

import           Control.Monad.Except

import           Data.Int
import qualified Data.Text as T

import           Database.PostgreSQL.Store
import           Database.PostgreSQL.Store.Query

-- |
anyColumnType :: ColumnType
anyColumnType =
	ColumnType "blob" True Nothing

-- |
data Release = Release {
	releaseName :: T.Text
} deriving (Show, Eq, Ord, Generic)

-- |
data Torrent = Torrent {
	torrentTitle :: T.Text,
	torrentURI   :: T.Text
} deriving (Show, Eq, Ord, Generic)

instance Entity Torrent

instance TableEntity Torrent where
	describeTableType _ =
		Table "Torrents"
		      [Column "title" anyColumnType,
		       Column "uri"   anyColumnType]

-- |
insertTorrent :: Torrent -> Errand Int64
insertTorrent torrent = do
	r <- query qry
	case r of
		[r] -> pure r
		_   -> throwError (UserError "Torrent upsertion returned 0 or more than 1 result")
	where
		qry =
			[pgsq| INSERT INTO @Torrent (title, uri)
			       VALUES ($torrent)
			       ON CONFLICT (title, uri) DO UPDATE SET title = @Torrent.title
			       RETURNING id |]

-- |
searchForTorrents :: [T.Text] -> Errand [(Torrent, Word)]
searchForTorrents tags =
	query [pgsq| SELECT #Torrent, COUNT(tags) AS score
	             FROM @Torrent, tags
	             WHERE id = torrent
	                   AND tag IN ($(insertCommaSeperated (map insertTag tags)))
	             GROUP BY id
	             ORDER BY score DESC |]
	where
		insertTag =
			insertEntity . T.toLower

-- |
associateTags :: Int64 -> [T.Text] -> Errand ()
associateTags tid tags =
	() <$ execute [pgsq| INSERT INTO tags (torrent, tag)
	                     VALUES $(insertCommaSeperated (map insertTag tags))
	                     ON CONFLICT (torrent, tag) DO NOTHING |]
	where
		insertTag tag =
			[pgsq| ($tid, $tag) |]
