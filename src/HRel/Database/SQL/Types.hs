module HRel.Database.SQL.Types
    ( Name (..) )
where

import Data.Text (Text)

newtype Name = Name {unName :: Text}
    deriving (Show, Eq, Ord)
