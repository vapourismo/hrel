module HRel.Database.Marshal
    ( Value
    , nullValue
    , ToValue (..)
    )
where

import qualified Data.ByteString.Char8 as Char8String
import           Data.Int
import           Data.Word

import qualified Database.PostgreSQL.LibPQ as LibPQ

import HRel.Database.Types

-- | @NULL@
nullValue :: Value
nullValue = Value Nothing

{-# INLINE nullValue #-}

-- | Types that can be sent to the database
class ToValue a where
    toValue :: a -> Value

instance ToValue Value where
    toValue = id

    {-# INLINE toValue #-}

showValue :: Show a => LibPQ.Oid -> a -> Value
showValue oid subject = Value (Just (oid, Char8String.pack (show subject), LibPQ.Text))

instance ToValue Int where
    toValue = showValue LibPQ.invalidOid

instance ToValue Int8 where
    toValue = showValue LibPQ.invalidOid

instance ToValue Int16 where
    toValue = showValue LibPQ.invalidOid

instance ToValue Int32 where
    toValue = showValue LibPQ.invalidOid

instance ToValue Int64 where
    toValue = showValue LibPQ.invalidOid

instance ToValue Word where
    toValue = showValue LibPQ.invalidOid

instance ToValue Word8 where
    toValue = showValue LibPQ.invalidOid

instance ToValue Word16 where
    toValue = showValue LibPQ.invalidOid

instance ToValue Word32 where
    toValue = showValue LibPQ.invalidOid

instance ToValue Word64 where
    toValue = showValue LibPQ.invalidOid
