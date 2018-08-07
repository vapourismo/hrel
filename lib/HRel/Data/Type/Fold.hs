{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module HRel.Data.Type.Fold where

type family Fold (p :: a -> b -> b) (xs :: [a]) (y :: b) :: b where
    Fold _ '[]       y = y
    Fold p (x ': xs) y = p x (Fold p xs y)

type family UnfoldXS (p :: a -> b -> b) (r :: b) :: [a] where
    UnfoldXS p (p a b) = a ': UnfoldXS p b
    UnfoldXS _ _       = '[]

type family UnfoldY (p :: a -> b -> b) (r :: b) :: b where
    UnfoldY p (p _ b) = UnfoldY p b
    UnfoldY _ b       = b

data Unfolded a b = Unfolded [a] b

type Unfold (p :: a -> b -> b) (r :: b) = 'Unfolded (UnfoldXS p r) (UnfoldY p r)
