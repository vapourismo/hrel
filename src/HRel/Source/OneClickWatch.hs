{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.OneClickWatch where

import Data.Char
import qualified Data.Text.Lazy as T

import Control.Monad
import Control.Applicative

import Network.URI

import HRel.Markup.Download
import HRel.URI

-- | Filter a blog post.
postFilter :: NodeFilterH [URI]
postFilter =
	relativeTag "div" $ do
		attrGuard "id" "content"
		forTag "div" . forTag "div" $ do
			attrGuard "class" "entry"
			forTag "p" . relativeTags "a" $ do
				href <- attr "href"
				inner <- text
				guard (T.strip inner == T.strip href)

				toURI (T.strip href)

-- | Filter a RSS feed.
rssFilter :: NodeFilterH [(,) T.Text [URI]]
rssFilter =
	relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
		appl <$> forTag "title" text
		     <*> (forTag "link" text >>= aggregateLinks)
	where
		appl t l =
			(T.intercalate "." (filter (not . T.null) (T.split isSpace t)), l)
		aggregateLinks url =
			continueWith' (T.unpack url) [] postFilter

-- Example Feeds:
--   "http://oneclickwatch.ws/feed/"
