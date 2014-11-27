{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import HRel.Database

main :: IO ()
main = do
	db <- connectToDatabase
	rs <- findNames db ["person", "of", "interest"]
	mapM_ print rs

