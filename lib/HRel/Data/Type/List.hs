{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module HRel.Data.Type.List
    ( Find
    , Has
    , Subset
    )
where

import Data.Kind (Constraint)

type family Find (x :: a) (xs :: [p]) :: b where
    Find x (_ x y ': _) = y
    Find x (_ ': xs)    = Find x xs

type family Has (needle :: a) (haystack :: [a]) where
    Has f (f ': fs) = f ~ f
    Has f (g ': fs) = Has f fs

type family Subset (lhs :: [a]) (rhs :: [a]) :: Constraint where
    Subset (l ': ls) rhs = (Has l rhs, Subset ls rhs)
    Subset '[]       _   = ()
