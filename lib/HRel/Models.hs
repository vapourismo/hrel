{-# LANGUAGE TemplateHaskell #-}

module HRel.Models (
	Release (..),
	Torrent (..)
) where

import qualified Data.Text as T
import           Database.PostgreSQL.Store

-- |
data Release = Release {
	releaseName :: T.Text
} deriving (Show, Eq, Ord)

mkTable ''Release [Unique ['releaseName]]

-- |
data Torrent = Torrent {
	torrentTitle :: T.Text,
	torrentURI   :: T.Text
} deriving (Show, Eq, Ord)

mkTable ''Torrent [Unique ['torrentTitle, 'torrentURI]]
