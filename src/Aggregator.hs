module Main (main) where

import HRel.Aggregators

main :: IO ()
main =
	aggregate
	>>= print . length
