module Main (main) where

import HRel.Favs.XRel

main :: IO ()
main = do
	n <- fetch "http://www.xrel.to/releases-usrss.html?u=20470&s=ee663473a8da8a161902c908326ebe1c&favs=1"
	print n
