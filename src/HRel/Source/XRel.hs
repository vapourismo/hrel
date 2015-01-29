{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.XRel (
	releaseInfoFilter,
	releaseInfoRequest
) where

import Control.Applicative

import qualified Data.Text as T
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
			"tv"    -> (Episode prodTitle <$> forTag "tv_season" (fmap (read . T.unpack) text)
			                              <*> forTag "tv_episode" (fmap (read . T.unpack) text))
			           <|> return (Episode prodTitle 0 0)
			"movie" -> return (Movie prodTitle)
			"game"  -> return (Game prodTitle)
			_       -> return Unknown

		return (Release relName prod)

-- | Generate the request instance
releaseInfoRequest :: B.ByteString -> IO Request
releaseInfoRequest rel =
	setQueryString [("dirname", Just rel)] <$> parseUrl "http://api.xrel.to/api/release/info.xml"
