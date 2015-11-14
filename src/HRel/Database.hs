{-# LANGUAGE OverloadedStrings #-}

module HRel.Database (
	-- * Connection
	Database,
	connectDatabase,
	closeDatabase,
	P.query,
	P.query_,
	P.execute,
	P.execute_,
	P.executeMany,
	P.Only (..),
	P.In (..),

	-- * URI
	URIField (..),

	-- * Feeds
	FeedRow (feedID, feedURI, feedLastUpdate),
	listFeeds,
	insertFeed
) where

import           Data.Time
import qualified Data.ByteString                      as B
import           URI.ByteString
import qualified Database.PostgreSQL.Simple           as P
import qualified Database.PostgreSQL.Simple.ToField   as P
import qualified Database.PostgreSQL.Simple.FromField as P

-- | Database connection
type Database = P.Connection

-- | Connect to the database.
connectDatabase :: IO Database
connectDatabase =
	P.connect P.defaultConnectInfo {
		P.connectUser = "hrel",
		P.connectDatabase = "hrel"
	}

-- | Close the database connection
closeDatabase :: Database -> IO ()
closeDatabase = P.close

-- | Wrapper for URI
newtype URIField = URIField { fromURIField :: URI }

instance P.FromField URIField where
	fromField f cnt = do
		bs <- P.fromField f cnt
		case parseURI laxURIParserOptions bs of
			Left _    -> P.returnError P.ConversionFailed f "Not a valid URI"
			Right uri -> pure (URIField uri)

instance P.ToField URIField where
	toField (URIField uri) = P.toField (serializeURI' uri)

-- | Feed row
data FeedRow = FeedRow {
	feedID         :: Int,
	feedURI        :: B.ByteString,
	feedLastUpdate :: UTCTime
} deriving (Show, Eq, Ord)

constructFeed :: (Int, B.ByteString, UTCTime) -> FeedRow
constructFeed (fid, uri, lastUpdate) = FeedRow fid uri lastUpdate

-- | Find all feeds.
listFeeds :: P.Connection -> IO [FeedRow]
listFeeds con =
	map constructFeed <$> P.query_ con "SELECT id, uri, lastUpdate FROM feeds"

-- | Insert a new feed.
insertFeed :: P.Connection -> URI -> IO (Maybe FeedRow)
insertFeed con uri =
	fmap extractResult (P.query con
	                            "INSERT INTO feeds (uri) values (?) returning id, uri, lastUpdate"
	                            (P.Only (URIField uri)))
	where
		extractResult (x : _) = Just (constructFeed x)
		extractResult []      = Nothing
