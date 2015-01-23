{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.XRel where

import Control.Applicative

import Data.Word
import qualified Data.Text.Lazy as T

import HRel.Markup.Node

-- | Release information
data Release
	= Movie T.Text T.Text
	| Television T.Text T.Text Word Word
	| Game T.Text T.Text
	| Unknown T.Text T.Text
	deriving (Show, Eq)

-- | Filter a response to "release/info".
releaseInfoFilter :: NodeFilter T.Text Release
releaseInfoFilter =
	relativeTag "release" $ do
		relName <- forTag "dirname" text

		(t, prodTitle) <- forTag "ext_info" $
			(,) <$> forTag "type" text
			    <*> forTag "title" text

		case T.strip (T.toLower t) of
			"movie" -> return (Movie relName prodTitle)
			"tv"    -> Television relName prodTitle <$> forTag "tv_season" (fmap (read . T.unpack) text)
			                                        <*> forTag "tv_episode" (fmap (read . T.unpack) text)
			"game"  -> return (Game relName prodTitle)
			_       -> return (Unknown relName prodTitle)
