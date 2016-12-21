{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HRel.Models

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

main :: IO ()
main = do
	db <- P.connectdb "postgres://hrel@localhost/hrelhaskell"
	res <- runErrand db $ do
		searchForTorrents ["continent", "7", "720p"]

	print res
