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
		let links' = filter isValidURI links
		in when (length links' > 0 && length names > 0) . void $
			insertGroup db names (filter isValidURI links)
