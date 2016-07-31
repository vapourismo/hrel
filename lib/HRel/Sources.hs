{-# LANGUAGE OverloadedStrings #-}

module HRel.Sources (
	pirateBaySource
) where

import           HRel.NodeFilter

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

-- | Pirate Bay RSS source.
pirateBaySource :: (Monad m) => NodeFilterT B.ByteString m [(T.Text, B.ByteString)]
pirateBaySource =
	forRelativeTag "rss" $ "channel" $/ "item" $// do
		title <- "title" $/ text
		uri <- "torrent" $/ "magnetURI" $/ text
		pure (T.strip (T.decodeUtf8 title), uri)
