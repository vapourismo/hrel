{-# LANGUAGE OverloadedStrings #-}

module HRel.Torrent (
	-- * Torrent
	Torrent (..),
) where

import Data.List
import qualified Data.Text as T

import Network.URI

import HRel.Units
import HRel.Release

-- | Torrent
data Torrent = Torrent {
	torrentRelease     :: Release,
	torrentSource      :: [URI],
	torrentContentSize :: Maybe Word
} deriving (Eq, Ord)

instance Show Torrent where
	show (Torrent rel uris size) =
		"Release '" ++ T.unpack (toText rel) ++ "'" ++
		maybe [] (\ s -> " (" ++ showAsBytes s ++ ")") size ++ "\n" ++
		intercalate "\n" (map (\ u -> " + " ++ show u) uris)
