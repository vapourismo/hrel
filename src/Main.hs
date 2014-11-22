{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Control.Monad
import Control.Monad.Trans.Maybe

import Network.URI
import Network.HTTP.Conduit

import HRel.Markup

main :: IO ()
main = do
	txt <- fmap T.decodeUtf8 (simpleHttp "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/")
	case runNodeFilter rssFilter (parseNode txt) of
		Just pairs -> forM_ pairs displayRelase
		Nothing    -> putStrLn "Found nothing"
	where
		rssFilter = relative . node "channel" . foreachNode "item" $ do
			title <- node "title" text
			urls <- foreachNode "enclosure" $
				attribute "url"
				>>= MaybeT . return . parseURI . T.unpack
			return (title, urls)

		displayRelase (title, uris) = do
			print (map (T.dropAround isSpace) (T.split (== '&') title))
			print uris
