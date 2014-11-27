{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Word
import Control.Monad
import HRel.Database

main :: IO ()
main = do
	db <- connectToDatabase
	groups <- query_ db "SELECT id FROM groups" :: IO [Only Word64]
	forM_ groups $ \p -> do
		names <- query db "SELECT fullName FROM names WHERE groupID = ?" p :: IO [Only String]
		links <- query db "SELECT uri FROM links WHERE groupID = ?" p :: IO [Only String]
		putStrLn "---"
		mapM_ (putStrLn . fromOnly) names
		mapM_ (putStrLn . fromOnly) links
