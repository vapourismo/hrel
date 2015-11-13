{-# LANGUAGE OverloadedStrings #-}
module HRel.Sources.TorrentInfo (
	TorrentInfo (..)
) where

import qualified Data.Text as T

import           Network.URI

import           HRel.Sources.ReleaseName

-- | Torrent information
data TorrentInfo = TorrentInfo {
	torrentInfoTitle       :: T.Text,
	torrentInfoNormalized  :: ReleaseName,
	torrentInfoSource      :: URI,
	torrentInfoContentSize :: Maybe Word
} deriving (Show, Eq, Ord)
