{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Network.Http.Client
import HRel.Sources.ReleaseFeeds

main :: IO ()
main =
	get "http://www.xrel.to/releases-usrss.html?u=20470&s=ee663473a8da8a161902c908326ebe1c&favs=1"
	    handleResponse
	where
		handleResponse res input =
			when (getStatusCode res == 200) $ do
				contents <- concatHandler res input
				print (parseReleaseFeed contents)
