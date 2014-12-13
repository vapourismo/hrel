{-# LANGUAGE OverloadedStrings #-}

module HRel.Favs.XRel (
	fetch
) where

import qualified Data.Text.Lazy as T

import HRel.Download
import HRel.Markup

xrelFeedFilter :: NodeFilter T.Text [T.Text]
xrelFeedFilter =
	relativeTag "feed" $
		foreachTag "entry" $
			forTag "title" text

-- | Fetch the release names for a favlist. URL must point to an Atom feed.
fetch :: String -> IO [T.Text]
fetch url =
	fmap (maybe [] id . runNodeFilter xrelFeedFilter) (downloadNode url)
