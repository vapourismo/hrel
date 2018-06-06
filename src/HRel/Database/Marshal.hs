module HRel.Database.Marshal
    ( Value
    , nullValue
    , Marshal (..) )
where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category
import Control.Monad

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
