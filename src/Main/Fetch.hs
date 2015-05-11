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
main =
	withDatabase $ \ db ->
		fetch dumps >>= mapM_ (insertTorrent db)
