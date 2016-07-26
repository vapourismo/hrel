module Main where

import HRel.Markup

main :: IO ()
main = do
	input <- readFile "test.xml"
	print (parseMarkup input)
