{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Concurrent

import HRel.Processing

main :: IO ()
main = withManifest $ \ mf -> do
	spawnWorkers mf
	processAllFeeds mf
	forever (threadDelay 1000 >> yield)
