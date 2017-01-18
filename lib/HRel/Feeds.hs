{-# LANGUAGE OverloadedStrings, RankNTypes, QuasiQuotes #-}

module HRel.Feeds (
	Feed (..),

	listFeeds,
	updateFeedTitle,
	associateSuitableTorrents,

	atomFilter,
	rssFilter,

	FeedError (..),

	FeedProducer,
	feedSource
) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource

import           Data.Conduit
import           Data.Conduit.Text

import           Data.Int
import qualified Data.Text as T

import           Network.HTTP.Client

import           Database.PostgreSQL.Store
import           Database.PostgreSQL.Store.Query

import           HRel.Monad
import           HRel.Network
import           HRel.Markup
import           HRel.NodeFilter
import           HRel.Names
import           HRel.Torrents

-- | Feed
data Feed = Feed {
	feedTitle    :: T.Text,
	feedContents :: [T.Text]
} deriving (Show, Eq, Ord)

-- |
listFeeds :: Errand [(Int64, T.Text, String)]
listFeeds =
	query [pgsq| SELECT id, title, url FROM feeds |]

-- |
updateFeedTitle :: Int64 -> T.Text -> Errand ()
updateFeedTitle fid title =
	() <$ execute [pgsq| UPDATE feeds SET title = $title WHERE id = $fid |]

-- |
associateSuitableTorrents :: Int64 -> T.Text -> Errand [Int64]
associateSuitableTorrents fid name =
	query [pgsq| WITH allResults AS (
	                 SELECT id, COUNT(tags) AS score
	                 FROM @Torrent, tags
	                 WHERE id = torrent
	                       AND tag IN ($(insertCommaSeperated (map insertEntity tags)))
	                 GROUP BY id
	                 ORDER BY score DESC
	             )
	             INSERT INTO feed_contents (feed, torrent)
	             SELECT $fid, id FROM allResults WHERE score >= $(length tags) |]
	where
		tags = parseTags name

-- | Atom feeds
atomFilter :: NodeFilter Feed
atomFilter =
	Feed <$> ("title" $/ T.strip <$> text)
	     <*> ("entry" $// "title" $/ T.strip <$> text)

-- | RSS feeds
rssFilter :: NodeFilter Feed
rssFilter =
	"channel" $/
		Feed <$> ("title" $/ T.strip <$> text)
		     <*> ("item" $// "title" $/ T.strip <$> text)

-- | Error during feed processing
data FeedError
	= HttpError HttpError
	| XmlError XmlError
	| NodeFilterError NodeFilterError
	| InvalidUrl String
	deriving (Show)

-- | Producer conduit
type FeedProducer m o = forall i. HRelT FeedError (ConduitM i o) m ()

-- | Retrieve the contents of a feed.
feedSource :: (MonadCatch m, MonadResource m) => Manager -> String -> FeedProducer m Feed
feedSource mgr url = do
	req <- lift $
		case parseRequest url of
			Just x  -> pure x
			Nothing -> throwError (InvalidUrl url)

	withHRelT HttpError (httpRequest mgr req)
		=$= decode utf8
		=$= withHRelT XmlError processXML
		=$= withHRelT NodeFilterError (filterNodes (rssFilter <|> atomFilter))
