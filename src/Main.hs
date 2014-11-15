{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Text.HTML.TagSoup
import Network.HTTP.Conduit

import HRel.XML

fetchTags :: String -> IO [Tag T.Text]
fetchTags = fmap (parseTags . T.decodeUtf8) . simpleHttp

showNode :: Int -> Node T.Text -> String
showNode n (Node t as cnt cs) =
	prefix ++ " " ++
	attrs ++ nl ++
	T.unpack cnt ++ "\n" ++
	rest
	where
		prefix = concat (replicate n "  ") ++ "+ " ++ T.unpack t
		nl = "\n" ++ replicate (length prefix + 1) ' '
		attrs = concat (intersperse nl (map (\(k, v) -> T.unpack k ++ ": " ++ T.unpack v) as))
		rest = concat (map (showNode (n + 1)) cs)

main :: IO ()
main = do
	tags <- fetchTags "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/"
	putStrLn (showNode 0 (head (toNodeList tags)))
