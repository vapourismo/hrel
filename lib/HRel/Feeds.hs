{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module HRel.Feeds (
	Feed (..),

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

import qualified Data.Text as T

import           Network.HTTP.Client

import           HRel.Monad
import           HRel.Network
import           HRel.Markup
import           HRel.NodeFilter


-- | Feed
data Feed = Feed {
	feedTitle    :: T.Text,
	feedContents ::  [T.Text]
} deriving (Show, Eq, Ord)

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
