{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.Encoding as T
-- import qualified Data.ByteString.Lazy as B

import HRel.Markup
import Network.HTTP.Conduit

main :: IO ()
main =
	simpleHttp "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/"
	>>= print . runNodeFilter rssFilter . parseNode
	where
		rssFilter =
			relative $ node "channel" $ foreachNode "item" $ do
				title <- node "title" text
				urls <- foreachNode "enclosure" (attribute "url")
				return (title, urls)
