{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Maybe

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Feeds

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings

	res <- runMaybeT (downloadFeed mgr "http://www.movie-blog.org/feed/")
	print res
