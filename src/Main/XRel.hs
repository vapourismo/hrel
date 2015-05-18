module Main where

import System.Environment

import Control.Monad.Reader

import Data.Conduit
import qualified Data.Conduit.List as C

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Source.XRel
import HRel.Source.KickAss

-- |
main :: IO ()
main = do
	args <- getArgs
	case args of
		[u, s, f] ->
			withManager tlsManagerSettings $ \ mgr ->
				mapM_ print <=< flip runReaderT mgr $ runConduit $
					xrelFavourites ("http://www.xrel.to/releases-usrss.html?u=" ++ u
					                ++ "&s=" ++ s
					                ++ "&favs=" ++ f)
						=$= kickAssReleaseSearch
						=$= C.consume
		_ -> do
			prog <- getProgName
			putStrLn ("Usage: " ++ prog ++ " <u param> <s param> <favs param>")
