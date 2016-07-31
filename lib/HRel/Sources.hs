{-# LANGUAGE OverloadedStrings #-}

module HRel.Sources (
	pirateBaySource
) where

import           HRel.NodeFilter
import           HRel.Models

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

-- | Pirate Bay RSS source.
pirateBaySource :: (Monad m) => NodeFilterT B.ByteString m [Torrent]
pirateBaySource =
	forRelativeTag "rss" $ "channel" $/ "item" $// do
		Torrent <$> ("title" $/ T.strip . T.decodeUtf8 <$> text)
		        <*> ("torrent" $/ "magnetURI" $/ T.strip . T.decodeUtf8 <$> text)
