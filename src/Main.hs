{-# LANGUAGE OverloadedStrings #-}

module Main where

import HRel.Database

main :: IO ()
main = do
	con <- connectDatabase

	rows <- listFeeds con
	print rows

	closeDatabase con
