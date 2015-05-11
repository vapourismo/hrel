{-# LANGUAGE OverloadedStrings #-}

module Main where

import HRel.Source
import HRel.Source.KickAss

import HRel.Database

-- | Source for dumps
dumps :: Aggregator Torrent
dumps = kickAssHourly

-- | Entry point
main :: IO ()
main = do
	rs <- fetch dumps
	withDatabase $ \ db ->
		mapM_ (insertTorrent db) rs
