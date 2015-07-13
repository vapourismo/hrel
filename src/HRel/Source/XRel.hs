{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HRel.Source.XRel (
	xrelFavourites
) where

import Data.List

import qualified Data.Conduit.List as C

import qualified Data.Text.Encoding as T

import HRel.Conduit
import HRel.Markup
import HRel.Release

-- |
xrelFavourites :: String -> HRelSource Release
xrelFavourites url =
	request url =$= fetch =$= C.map T.decodeUtf8
	            =$= markup xmlFilter =$= C.map nub =$= C.concat
	where
		xmlFilter =
			relativeTag "feed" (foreachTag "entry" (forTag "title" (makeRelease <$> text)))
