module Main where

import qualified Data.Conduit.List as C

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Source.XRel
import HRel.Conduit

main :: IO ()
main = do
	withManager tlsManagerSettings $ \ mgr -> do
		contents <- runHRelConduit mgr (xrelFavourites xrelURL =$= C.consume)
		print contents
		print (length contents)

	where
		xrelURL = "http://www.xrel.to/releases-usrss.html?u=20470&s=ee663473a8da8a161902c908326ebe1c&favs=1"
