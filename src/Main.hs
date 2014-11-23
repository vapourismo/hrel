{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified HRel.Aggregator.DDLValleyRocks as DDLValley

main :: IO ()
main =
	DDLValley.aggregate "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/"
	>>= print
