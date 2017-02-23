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
import           Database.PostgreSQL.Store.Entity

import           HRel.Names

-- | Torrent
data Torrent = Torrent {
	torrentTitle :: T.Text,
	torrentURI   :: T.Text
} deriving (Show, Eq, Ord, Generic)

instance Entity Torrent

instance TableEntity Torrent where
	describeTableType =
		Tagged (Table "torrents" ["title", "uri"])

instance ToJSON Torrent where
	toJSON (Torrent title uri) =
		object ["title" .= title, "uri" .= uri]

-- | Insert a comma-seperated list where each element is surrounded with parentheses.
insertTuples :: (Entity e) => [e] -> QueryGenerator a
insertTuples es =
	joinGens "," (map (\ e -> mconcat ["(", embedEntity e, ")"]) es)

-- | Insert many torrents.
insertTorrents :: [Torrent] -> Errand Int
insertTorrents [] = pure 0
insertTorrents torrents = do
	inserted <- query [pgQuery| INSERT INTO @Torrent (title, uri)
	                            VALUES $(insertTuples torrents)
	                            ON CONFLICT (title, uri) DO NOTHING
	                            RETURNING id, title |]

	case concatMap buildTagMap inserted of
		[]   -> pure ()
		tags -> () <$ execute [pgQuery| INSERT INTO tags (torrent, tag)
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
	query [pgQuery| WITH allResults AS (
	                    SELECT #Torrent, COUNT(tags) AS score
	                    FROM @Torrent, tags
	                    WHERE id = torrent
	                          AND tag IN ($(joinGens "," (map insertTag tags)))
	                    GROUP BY id
	                    ORDER BY score DESC, insertion DESC
	                ),
	                scores AS (
	                    SELECT MAX(score) AS maxScore FROM allResults
	                )
	                SELECT #Torrent(r)
	                FROM allResults r, scores
	                WHERE r.score >= maxScore
	                LIMIT 500 |]
	where
		insertTag =
			embedEntity . T.toLower
