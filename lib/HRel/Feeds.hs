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

import qualified Data.Text          as T

import           Network.HTTP.Client

import           HRel.NodeFilter
import           HRel.Network
import           HRel.Markup

-- | Contents of a feed
data FeedContents = FeedContents T.Text [T.Text]
	deriving (Show, Eq, Ord)

-- | Atom feeds
atomNodeFilter :: (Monad m) => NodeFilterT T.Text m FeedContents
atomNodeFilter =
	forRelativeTag "feed" $ do
		FeedContents <$> ("title" $/ T.strip <$> text)
		             <*> ("entry" $// "title" $/ T.strip <$> text)

-- | RSS feeds
rssNodeFilter :: (Monad m) => NodeFilterT T.Text m FeedContents
rssNodeFilter =
	forRelativeTag "rss" $ "channel" $/
		FeedContents <$> ("title" $/ T.strip <$> text)
		             <*> ("item" $// "title" $/ T.strip <$> text)

-- | Atom or RSS feeds
feedNodeFilter :: (Monad m) => NodeFilterT T.Text m FeedContents
feedNodeFilter = atomNodeFilter <|> rssNodeFilter

-- | Download a parse a feed.
downloadFeed :: Manager -> String -> MaybeT IO FeedContents
downloadFeed mgr url = do
	cnt <- download_ mgr url
	node <- MaybeT (pure (parseMarkup_ cnt))
	runNodeFilterT_ node feedNodeFilter
