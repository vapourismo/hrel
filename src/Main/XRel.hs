module Main where

import System.Environment

import HRel.Source
import HRel.Source.XRel
import HRel.Source.KickAss

main :: IO ()
main = do
	args <- getArgs
	case args of
		[u, s, f] ->
			fetch (fetcher (read u) s (read f)) >>= mapM_ print

		_ -> do
			prog <- getProgName
			putStrLn ("Usage: " ++ prog ++ " <user> <session> <list nr>")

	where
		fetcher u s f = do
			r <- xrelFavourites u s f
			t <- kickAssReleaseSearch r
			if torrentRelease t == r then
				pure t
			else
				mempty
