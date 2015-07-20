{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.Feeds (
	fromAtomTitles
) where

import           Control.Monad.Trans
import           Control.Monad.Catch

import           Data.List
import qualified Data.Conduit.List  as C
import qualified Data.Text.Encoding as T

import           Network.HTTP.Client

import           HRel.Conduit
import           HRel.Markup
import           HRel.Release

-- | Extract releases from Atom entry titles.
fromAtomTitles :: (MonadIO m, MonadThrow m) => Manager -> String -> Source m Release
fromAtomTitles mgr url =
	request url =$= fetch mgr =$= C.map T.decodeUtf8
	            =$= markup xmlFilter =$= C.map nub =$= C.concat
	where
		xmlFilter =
			reverse <$> relativeTag "feed" (foreachTag "entry" (forTag "title" (makeRelease <$> text)))
