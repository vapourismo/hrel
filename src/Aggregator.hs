module Main (main) where

import Control.Monad

import HRel.Aggregators
import HRel.Database
import HRel.Tools

main :: IO ()
main = do
	db <- connectToDatabase
	rels <- aggregate
	forM_ rels $ \(names, links) ->
		insertGroup db names (filter isValidURI links)
