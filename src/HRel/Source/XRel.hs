{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.XRel (
	xrelFavourites
) where

import Control.Monad.Trans
import Control.Monad.Catch

import Data.List

import Data.Conduit
import qualified Data.Conduit.List as C

import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL

import HRel.Fetch
import HRel.Markup
import HRel.Release
import HRel.Conduit

-- | Retrieve the xRel favourite list at the given URL.
xrelFavourites :: (MonadIO m, MonadThrow m) => String -> Source (FetchT m) Release
xrelFavourites url =
	request url
		=$= fetch
		=$= C.map (T.decodeUtf8 . BL.toStrict)
		=$= markup rssFilter
		=$= C.map nub
		=$= C.concat
	where
 		rssFilter =
 			relativeTag "feed" (foreachTag "entry" (forTag "title" (makeRelease <$> text)))
