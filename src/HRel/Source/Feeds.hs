{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.Feeds (
	fromRSSTitles
) where

import Data.List
import qualified Data.Conduit.List as C
import qualified Data.Text.Encoding as T

import HRel.Conduit
import HRel.Markup
import HRel.Release

-- | Extract releases from RSS entry titles.
fromRSSTitles :: String -> HRelSource Release
fromRSSTitles url =
	request url =$= fetch =$= C.map T.decodeUtf8
	            =$= markup xmlFilter =$= C.map nub =$= C.concat
	where
		xmlFilter =
			relativeTag "feed" (foreachTag "entry" (forTag "title" (makeRelease <$> text)))
