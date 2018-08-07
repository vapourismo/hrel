module HRel.Database.Marshal
    ( Value
    , nullValue
    , Marshal (..) )
where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category
import Control.Monad

import qualified Data.ByteString.Char8 as Char8String
import           Data.Int
import           Data.Word

import qualified Database.PostgreSQL.LibPQ as LibPQ

import HRel.Database.Query
import HRel.Database.Types

-- | @NULL@
nullValue :: Value
nullValue = Value Nothing

{-# INLINE nullValue #-}

-- | Types that can be sent to the database
class Marshal a where
    marshal :: QueryRecipe a Value

instance Marshal Value where
    marshal = id

    {-# INLINE marshal #-}

instance Marshal a => Marshal (Maybe a) where
    marshal =
        fixNothing <$> first marshal . arr (join (,))
        where
            fixNothing (_    , Nothing) = nullValue
            fixNothing (value, _      ) = value

    {-# INLINE marshal #-}

showValue :: Show a => LibPQ.Oid -> a -> Value
showValue oid subject = Value (Just (oid, Char8String.pack (show subject), LibPQ.Text))

instance Marshal Int where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Int8 where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Int16 where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Int32 where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Int64 where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Word where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Word8 where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Word16 where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Word32 where
    marshal = arr (showValue LibPQ.invalidOid)

instance Marshal Word64 where
    marshal = arr (showValue LibPQ.invalidOid)
