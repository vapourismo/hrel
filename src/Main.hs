{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Text.HTML.TagSoup
import HRel.XML

import Network.HTTP.Conduit

fetchContents :: String -> IO T.Text
fetchContents = fmap T.decodeUtf8 . simpleHttp

main :: IO ()
main = do
	txt <- fetchContents "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/"
	case toNodeList (parseTags txt) of
		[x] -> print $ runNodeFilter rssFilter x where
			rssFilter =
				relative $ node "channel" $ foreachNode "item" $ do
					title <- node "title" text
					urls <- foreachNode "enclosure" (attribute "url")
					return (title, urls)
		_ -> putStrLn "Too many nodes"
