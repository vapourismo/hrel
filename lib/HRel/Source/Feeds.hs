{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.Feeds (
	fromAtomTitles
) where

import           Control.Monad.Trans
import           Control.Monad.Catch

import           Data.List
import qualified Data.Conduit.List   as C
import qualified Data.Text.Encoding  as T

import           Network.HTTP.Client

import           HRel.Conduit
import           HRel.Markup
import           HRel.Data.Release

-- | Extract releases from Atom entry titles.
fromAtomTitles :: (MonadIO m, MonadThrow m) => Manager -> String -> Source m ReleaseName
fromAtomTitles mgr url =
	request url =$= fetch mgr =$= C.map T.decodeUtf8
	            =$= markup rootFilter =$= C.map nub =$= C.concat
	where
		titleFilter =
			normalizeReleaseName <$> text

		rootFilter =
			reverse <$> relativeTag "feed" (foreachTag "entry" (forTag "title" titleFilter))
