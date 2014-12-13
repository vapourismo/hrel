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

fetch :: String -> IO [T.Text]
fetch url = do
	cnts <- downloadNode url
	return (maybe [] id (runNodeFilter xrelFeedFilter cnts))
