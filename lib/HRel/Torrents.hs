{-# LANGUAGE OverloadedStrings, DeriveGeneric, QuasiQuotes #-}

module HRel.Torrents (
	Torrent (..),
	insertTorrents,
	searchForTorrents
) where

import           GHC.Generics

import           Data.Int
import           Data.Tagged
import           Data.Aeson
import qualified Data.Text as T

import           Database.PostgreSQL.Store
import           Database.PostgreSQL.Store.Query

import           HRel.Names

-- | Column type wildcard
anyColumnType :: ColumnType
anyColumnType =
	ColumnType "blob" True Nothing

-- | Torrent
data Torrent = Torrent {
	torrentTitle :: T.Text,
	torrentURI   :: T.Text
} deriving (Show, Eq, Ord, Generic)

instance Entity Torrent

instance TableEntity Torrent where
	describeTableType =
		Tagged (Table "torrents" [Column "title" anyColumnType,
		                          Column "uri"   anyColumnType])

instance ToJSON Torrent where
	toJSON (Torrent title uri) =
		object ["title" .= title, "uri" .= uri]

-- | Insert a comma-seperated list where each element is surrounded with parentheses.
insertTuples :: (Entity e) => [e] -> QueryBuilder
insertTuples es =
	insertCommaSeperated (map (\ e -> insertCode "(" >> insertEntity e >> insertCode ")") es)

-- | Insert many torrents.
insertTorrents :: [Torrent] -> Errand Int
insertTorrents [] = pure 0
insertTorrents torrents = do
	inserted <- query [pgsq| INSERT INTO @Torrent (title, uri)
	                         VALUES $(insertTuples torrents)
	                         ON CONFLICT (title, uri) DO NOTHING
	                         RETURNING id, title |]

	case concatMap buildTagMap inserted of
		[]   -> pure ()
		tags -> () <$ execute [pgsq| INSERT INTO tags (torrent, tag)
		                             VALUES $(insertTuples tags)
		                             ON CONFLICT (torrent, tag) DO NOTHING |]

	pure (length inserted)
	where
		buildTagMap :: (Int64, T.Text) -> [(Int64, T.Text)]
		buildTagMap (tid, title) = map ((,) tid) (parseTags title)

-- | For torrents with the given tags.
searchForTorrents :: [T.Text] -> Errand [Torrent]
searchForTorrents [] = pure []
searchForTorrents tags =
	query [pgsq| WITH allResults AS (
	                 SELECT #Torrent, COUNT(tags) AS score
	                 FROM @Torrent, tags
	                 WHERE id = torrent
	                       AND tag IN ($(insertCommaSeperated (map insertTag tags)))
	                 GROUP BY id
	                 ORDER BY score DESC, insertion DESC
	             ),
	             scores AS (
	                 SELECT MAX(score) AS maxScore FROM allResults
	             )
	             SELECT #Torrent(r) FROM allResults r, scores WHERE r.score >= maxScore |]
	where
		insertTag =
			insertEntity . T.toLower
