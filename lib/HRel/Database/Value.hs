module HRel.Database.Value
    ( Value (..)
    , nullValue
    , ToValue (..)
    )
where

import qualified Data.ByteString.Char8 as ByteString
import           Data.Int
import           Data.Word

import Database.PostgreSQL.LibPQ (Format (Text), Oid, invalidOid)

-- | Raw value that will be exchanged with the database
newtype Value = Value {unValue :: Maybe (Oid, ByteString.ByteString, Format)}

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

showValue :: Show a => Oid -> a -> Value
showValue oid subject = Value (Just (oid, ByteString.pack (show subject), Text))

instance ToValue Int where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Int8 where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Int16 where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Int32 where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Int64 where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Word where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Word8 where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Word16 where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Word32 where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}

instance ToValue Word64 where
    toValue = showValue invalidOid

    {-# INLINE toValue #-}
