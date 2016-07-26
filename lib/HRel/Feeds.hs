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

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Network.HTTP.Client

import           HRel.NodeFilter
import           HRel.Network
import           HRel.Markup

-- | Contents of a feed
data FeedContents = FeedContents T.Text [T.Text]
	deriving (Show, Eq, Ord)

-- | Atom feeds
atomNodeFilter :: (Monad m) => NodeFilterT B.ByteString m FeedContents
atomNodeFilter =
	"feed" $/ do
		title <- "title" $/ text
		items <- "entry" $// "title" $/ text

		pure (FeedContents (T.decodeUtf8 title) (map T.decodeUtf8 items))

-- | RSS feeds
rssNodeFilter :: (Monad m) => NodeFilterT B.ByteString m FeedContents
rssNodeFilter =
	"rss" $/ "channel" $/ do
		title <- "title" $/ text
		items <- "item" $// "title" $/ text

		pure (FeedContents (T.decodeUtf8 title) (map T.decodeUtf8 items))

-- | Atom or RSS feeds
feedNodeFilter :: (Monad m) => NodeFilterT B.ByteString m FeedContents
feedNodeFilter = atomNodeFilter <|> rssNodeFilter

-- | Download a parse a feed.
downloadFeed :: Manager -> String -> MaybeT IO FeedContents
downloadFeed mgr url = do
	cnt <- download_ mgr url
	node <- MaybeT (pure (parseMarkup_ cnt))
	runNodeFilterT_ node feedNodeFilter
