module Main (main) where

import Control.Monad

import HRel.Aggregators
import HRel.Database

main :: IO ()
main = do
	db <- connectToDatabase
	rels <- aggregate
	forM_ rels $ \(names, links) ->
		insertGroup db names links
