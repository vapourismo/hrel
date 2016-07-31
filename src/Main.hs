{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Maybe
import Control.Monad.Reader

import HRel.Markup
import HRel.Network
import HRel.NodeFilter
import HRel.Sources

import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings
	res <- downloadMarkup mgr "https://thepiratebay.org/rss/top100/0" pirateBaySource

	print res
