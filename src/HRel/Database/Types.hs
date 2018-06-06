
module HRel.Database.Types
    ( Value (..) )
where

import Data.ByteString (ByteString)

import qualified Database.PostgreSQL.LibPQ as LibPQ

-- | Raw value that will be exchanged with the database
newtype Value = Value {unValue :: Maybe (LibPQ.Oid, ByteString, LibPQ.Format)}
