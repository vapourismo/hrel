{-# LANGUAGE TemplateHaskell #-}

module HRel.Models (
	Release (..)
) where

import qualified Data.Text as T
import           Database.PostgreSQL.Store

data Release = Release {
	releaseName :: T.Text
} deriving (Show, Eq, Ord)

mkTable ''Release [Unique ['releaseName]]
