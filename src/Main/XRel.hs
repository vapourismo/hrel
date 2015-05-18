module Main where

import System.Environment

import Control.Monad.Reader

import Data.Conduit
import qualified Data.Conduit.List as C

import Network.HTTP.Client.TLS

import HRel.Fetch

import HRel.Source.XRel
import HRel.Source.KickAss

-- |
main :: IO ()
main = do
	args <- getArgs
	case args of
		[u, s, f] ->
			mapM_ print <=< flip runFetchT tlsManagerSettings $ runConduit $
				xrelFavourites ("http://www.xrel.to/releases-usrss.html?u=" ++ u
				                ++ "&s=" ++ s
				                ++ "&favs=" ++ f)
					=$= kickAssReleaseSearch
					=$= C.consume
		_ -> do
			prog <- getProgName
			putStrLn ("Usage: " ++ prog ++ " <u param> <s param> <favs param>")
