{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HRel.Database.SQL.Types
    ( Name (..)
    , Query
    )
where

import Data.String (IsString)
import Data.Text   (Text)

import Database.PostgreSQL.Simple (Query)

newtype Name = Name {unName :: Text}
    deriving (Show, Eq, Ord, IsString)
