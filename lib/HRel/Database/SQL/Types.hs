{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HRel.Database.SQL.Types
    ( Name (..)
    , Code (..)
    )
where

import Data.String (IsString)
import Data.Text   (Text)

newtype Name = Name {unName :: Text}
    deriving (Show, Eq, Ord, IsString)

newtype Code = Code {unCode :: Text}
    deriving (Show, Eq, Ord, IsString, Semigroup, Monoid)
