{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Markup
import HRel.NodeFilter
import HRel.Feeds
import HRel.Network

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings

	Just cnt <- download mgr "http://www.movie-blog.org/feed/"
	let [node] = parseMarkup cnt

	res <- runNodeFilterT node feedNodeFilter
	print res
