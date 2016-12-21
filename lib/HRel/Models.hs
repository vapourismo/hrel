{-# LANGUAGE OverloadedStrings, DeriveGeneric, QuasiQuotes #-}

module HRel.Models (
	Release (..),

	Torrent (..),
	qInsertTorrent,
	insertTorrent
) where

import           GHC.Generics

import           Control.Monad.Except

import           Data.Int
import qualified Data.Text as T

import           Database.PostgreSQL.Store

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
qInsertTorrent :: Torrent -> Query Int64
qInsertTorrent torrent =
	[pgsq| INSERT INTO @Torrent (title, uri)
	       VALUES ($torrent)
	       ON CONFLICT (title, uri) DO UPDATE SET title = @Torrent.title
	       RETURNING id |]

-- |
insertTorrent :: Torrent -> Errand Int64
insertTorrent torrent = do
	r <- query (qInsertTorrent torrent)
	case r of
		[r] -> pure r
		_   -> throwError (UserError "Torrent upsertion returned 0 or more than 1 result")
