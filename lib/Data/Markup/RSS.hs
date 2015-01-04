{-# LANGUAGE OverloadedStrings #-}

module Data.Markup.RSS (
	forRSSChannel,
	foreachRSSChannel
) where

import Data.String
import Data.Markup.Node

-- | Navigate into one RSS channel.
forRSSChannel :: (Functor m, Monad m, IsString t, Eq t)
              => NodeFilterT t m a -> NodeFilterT t m a
forRSSChannel = relativeTag "rss" . forTag "channel"

-- | Iterate through all RSS channels.
foreachRSSChannel :: (Functor m, Monad m, IsString t, Eq t)
                  => NodeFilterT t m a -> NodeFilterT t m [a]
foreachRSSChannel = relativeTag "rss" . foreachTag "channel"
