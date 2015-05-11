{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import System.Environment

import HRel.Source
import HRel.Database

main :: IO ()
main = do
	withDatabase $ \ db ->
		getArgs
			>>= findTorrents db . makeRelease . T.intercalate " " . map T.pack
			>>= mapM_ (mapM_ print . torrentSource)
