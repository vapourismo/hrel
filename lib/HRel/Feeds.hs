{-# LANGUAGE OverloadedStrings, RankNTypes, QuasiQuotes #-}

module HRel.Feeds (
	Feed (..),

	insertFeed,
	listFeeds,
	listFeedContents,
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
import           Database.PostgreSQL.Store.Entity

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
insertFeed :: T.Text -> Errand Int64
insertFeed url = do
	r <- query [pgQuery| INSERT INTO feeds (url)
	                     VALUES ($url)
	                     ON CONFLICT (url) DO UPDATE SET url = feeds.url
	                     RETURNING id |]
	case r of
		[fid] -> pure fid
		_     -> throwError (UserError "Invalid number of feeds inserted (not 1)")

-- |
listFeeds :: Errand [(Int64, T.Text, String)]
listFeeds =
	query [pgQuery| SELECT id, title, url FROM feeds |]

-- |
listFeedContents :: Int64 -> Errand [Torrent]
listFeedContents fid =
	query [pgQuery| SELECT #Torrent(t)
	                FROM @Torrent t JOIN feed_contents fc ON t.id = fc.torrent
	                WHERE fc.feed = $fid
	                ORDER BY t.insertion DESC
	                LIMIT 500 |]

-- |
updateFeedTitle :: Int64 -> T.Text -> Errand ()
updateFeedTitle fid title =
	() <$ execute [pgQuery| UPDATE feeds SET title = $title WHERE id = $fid |]

-- |
associateSuitableTorrents :: Int64 -> T.Text -> Errand ()
associateSuitableTorrents fid name =
	() <$ execute [pgQuery| WITH allResults AS (
	                            SELECT id, COUNT(tags) AS score
	                            FROM @Torrent, tags
	                            WHERE id = torrent
	                                  AND tag IN ($(joinGens "," (map embedEntity tags)))
	                            GROUP BY id
	                            ORDER BY score DESC
	                        )
	                        INSERT INTO feed_contents (feed, torrent)
	                        SELECT $fid, id FROM allResults WHERE score >= $(embedEntity (length tags))
	                        ON CONFLICT (feed, torrent) DO NOTHING |]
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
