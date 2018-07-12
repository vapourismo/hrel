{-# LANGUAGE DeriveGeneric #-}

module HRel.Network.Types
    ( FeedProcessRequest (..) )
where

import GHC.Generics (Generic (..))

import Data.Aeson (FromJSON (..), ToJSON (..))

newtype FeedProcessRequest =
    FeedProcessRequest String
    deriving (Show, Eq, Ord, Generic)

instance FromJSON FeedProcessRequest

instance ToJSON FeedProcessRequest
