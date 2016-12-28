{-# LANGUAGE OverloadedStrings #-}

module HRel.Feeds (
	FeedContents (..),
	atomNodeFilter,
	rssNodeFilter,
	feedNodeFilter,

	downloadFeed
) where

import           Control.Applicative
import           Control.Monad.Trans.Maybe

import qualified Data.Text as T

import           Network.HTTP.Client

import           HRel.NodeFilter
import           HRel.Network

-- | Contents of a feed
data FeedContents = FeedContents T.Text [T.Text]
	deriving (Show, Eq, Ord)

-- | Atom feeds
atomNodeFilter :: NodeFilter FeedContents
atomNodeFilter =
	forRelativeTag "feed" $ do
		FeedContents <$> ("title" $/ T.strip <$> text)
		             <*> ("entry" $// "title" $/ T.strip <$> text)

-- | RSS feeds
rssNodeFilter :: NodeFilter FeedContents
rssNodeFilter =
	forRelativeTag "rss" $ "channel" $/
		FeedContents <$> ("title" $/ T.strip <$> text)
		             <*> ("item" $// "title" $/ T.strip <$> text)

-- | Atom or RSS feeds
feedNodeFilter :: NodeFilter FeedContents
feedNodeFilter = atomNodeFilter <|> rssNodeFilter

-- | Download a parse a feed.
downloadFeed :: Manager -> String -> MaybeT IO FeedContents
downloadFeed mgr url = do
	downloadMarkup_ mgr url feedNodeFilter
