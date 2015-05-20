module Main where

import System.Environment

import Data.Conduit

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel
import HRel.Database

-- | Entry Point
main :: IO ()
main = do
	args <- getArgs
	case args of
		[u, s, f] ->
			withManager tlsManagerSettings $ \ mgr ->
				withDatabase $ \ db -> do
					runConduit $
						xrelFavourites ("http://www.xrel.to/releases-usrss.html?u=" ++ u
						                ++ "&s=" ++ s
						                ++ "&favs=" ++ f)
						               mgr
							=$= watchRelease db
							=$= kickAssReleaseSearch mgr
							=$= pairTorrents db
					updateNewTorrents db

		_ -> do
			prog <- getProgName
			putStrLn ("Usage: " ++ prog ++ " <u param> <s param> <favs param>")
