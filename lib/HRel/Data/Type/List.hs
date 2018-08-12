{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HRel.Data.Type.List
    ( Find
    )
where

import GHC.TypeLits

type family Find (x :: a) (xs :: [p]) :: b where
    Find x '[]          = TypeError ('Text "Can't find key " ':<>: 'ShowType x)
    Find x (_ x y ': _) = y
    Find x (_ ': xs)    = Find x xs
