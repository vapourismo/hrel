{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.XRel (
	releaseInfoFilter,
	releaseInfoRequest
) where

import Control.Applicative

import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B

import Network.HTTP.Client

import HRel.Markup.Node
import HRel.Release

-- | Filter a response to "release/info".
releaseInfoFilter :: NodeFilter T.Text Release
releaseInfoFilter =
	relativeTag "release" $ do
		relName <- forTag "dirname" text

		(t, prodTitle) <- forTag "ext_info" $
			(,) <$> forTag "type" text
			    <*> forTag "title" text

		case T.strip (T.toLower t) of
			"tv"    -> Television relName prodTitle <$> forTag "tv_season" (fmap (read . T.unpack) text)
			                                        <*> forTag "tv_episode" (fmap (read . T.unpack) text)
			"movie" -> return (Movie relName prodTitle)
			"game"  -> return (Game relName prodTitle)
			_       -> return (Unknown relName prodTitle)

-- | Generate the URL
releaseInfoRequest :: B.ByteString -> IO Request
releaseInfoRequest rel =
	setQueryString [("dirname", Just rel)] <$> parseUrl "http://api.xrel.to/api/release/info.xml"
