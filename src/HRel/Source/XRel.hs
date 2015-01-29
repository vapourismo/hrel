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

		prod <- case T.strip (T.toLower t) of
			"tv"    -> fmap Just $
				Episode prodTitle <$> forTag "tv_season" (fmap (read . T.unpack) text)
				                  <*> forTag "tv_episode" (fmap (read . T.unpack) text)
			"movie" -> return (Just (Movie prodTitle))
			"game"  -> return (Just (Game prodTitle))
			_       -> return (Nothing)

		return (Release relName prod)

-- | Generate the URL
releaseInfoRequest :: B.ByteString -> IO Request
releaseInfoRequest rel =
	setQueryString [("dirname", Just rel)] <$> parseUrl "http://api.xrel.to/api/release/info.xml"
