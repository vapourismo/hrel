module Main where

import qualified Data.Conduit.List as C

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           HRel.Source.Feeds
import           HRel.Source.KickAssTorrents
import           HRel.Conduit

main :: IO ()
main =
	withManager tlsManagerSettings $ \ mgr -> do
		contents <- runHRelConduit mgr (fromRSSTitles xrelURL =$= kickAssSearch =$= C.consume)
		mapM_ print contents

	where
		xrelURL = "http://www.xrel.to/releases-usrss.html?u=20470&s=ee663473a8da8a161902c908326ebe1c&favs=1"
