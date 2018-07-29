{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module HRel.Data.Product
    ( Product (Nil, (:*))
    , empty
    , singleton
    , uncons

    , MakesProduct
    , mkProduct
    )
where

import GHC.Exts (Any, Proxy#, proxy#, unsafeCoerce#)

import           Data.List   (intercalate)
import qualified Data.Vector as Vector

import HRel.Data.Type.Fold

newtype Product (xs :: [*]) = Product {unProduct :: Vector.Vector Any}

pattern Nil :: Product '[]
pattern Nil <- _ where Nil = Product Vector.empty

infixr 7 :*

pattern (:*) :: x -> Product xs -> Product (x ': xs)
pattern x :* xs <-
    (uncons -> (x, xs))
    where
        x :* xs = Product (Vector.cons (unsafeCoerce# x) (unProduct xs))

class ShowProduct xs where
    showProductElements :: Proxy# xs -> Vector.Vector Any -> [String]

instance ShowProduct '[] where
    showProductElements _ _ = []

instance (Show x, ShowProduct xs) => ShowProduct (x ': xs) where
    showProductElements _ vec =
        show (unsafeCoerce# (Vector.unsafeHead vec) :: x)
        : showProductElements (proxy# :: Proxy# xs) (Vector.tail vec)

instance ShowProduct xs => Show (Product xs) where
    show (Product vec) =
        '{' : intercalate ", " (showProductElements (proxy# :: Proxy# xs) vec) ++ "}"

empty :: Product '[]
empty = Product Vector.empty

singleton :: a -> Product '[a]
singleton = Product . Vector.singleton . unsafeCoerce#

uncons :: Product (x ': xs) -> (x, Product xs)
uncons (Product vec) =
    ( unsafeCoerce# (Vector.unsafeHead vec)
    , Product (Vector.tail vec)
    )

type Biject (xs :: [*]) a f =
    ( 'Unfolded xs a ~ Unfold (->) f
    , f ~ Fold (->) xs a
    )

class Biject xs a f => WithProduct (xs :: [*]) a f where
    build :: (Product xs -> a) -> f

instance Biject '[] a a => WithProduct '[] a a where
    build = ($ empty)

instance WithProduct xs a f => WithProduct (x ': xs) a (x -> f) where
    build f x = build (\ xs -> f (x :* xs))

type MakesProduct xs f = WithProduct xs (Product xs) f

mkProduct :: MakesProduct xs f => f
mkProduct = build id
