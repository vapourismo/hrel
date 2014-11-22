{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B8

import Text.StringLike

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Network.URI
import Network.HTTP.Conduit

import HRel.Markup

-- type Source = IO [([B8.ByteString], [URI])]

-- toURI :: (Monad m, StringLike s) => s -> MaybeT m URI
-- toURI = MaybeT . return . parseURI . toString

-- ddlvalleySource :: Source
-- ddlvalleySource = do
-- 	maybe [] id . runNodeFilter rssFilter . parseNode
-- 	<$> simpleHttp "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/"
-- 	where
-- 		rssFilter =
-- 			relative . node "channel"
-- 			         $ foreachNode "item" targetNode
-- 		targetNode =
-- 			assocName <$> node "title" text
-- 			          <*> foreachNode "enclosure" (attribute "url" >>= toURI)
-- 		assocName title uris =
-- 			(map (B8.filter (not . isSpace)) (B8.split '&' title),
-- 			 uris)

main :: IO ()
main = do
	txt <- simpleHttp "http://www.ddlvalley.rocks/z-nation-s01e11-720p-hdtv-x264-dimension/"
	print (runNodeFilter htmlFilter (parseNode txt))
	where
		hasAttr k v = attr k >>= guard . (v ==)
		htmlFilter =
			relativeNode $ forTag "div" $ do
				hasAttr "class" "cont cl"
				relativeTags "a" $ attr "href"
