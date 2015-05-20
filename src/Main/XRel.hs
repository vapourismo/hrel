module Main where

import System.Environment

import Data.Conduit
import qualified Data.Conduit.List as C

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
					runConduit (xrel u s f mgr =$= watchRelease db =$= C.sinkNull)
					updateNewTorrents db
					runConduit (activeReleases db =$= kickAssReleaseSearch mgr =$= pairTorrents db)

		_ -> do
			prog <- getProgName
			putStrLn ("Usage: " ++ prog ++ " <u param> <s param> <favs param>")

	where
		xrel u s f = xrelFavourites ("http://www.xrel.to/releases-usrss.html?u=" ++ u ++ "&s=" ++ s ++ "&favs=" ++ f)
