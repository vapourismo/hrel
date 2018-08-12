{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module HRel.Database.SQL.Columns
    ( Columns
    , buildColumns

    , Column (..)
    , ColumnsOf

    , Selectable
    , toColumns
    , allColumns
    , Field
    , singleton

    , HasColumn (..)
    , (!)

    , Label (..)
    )
where

import GHC.OverloadedLabels
import GHC.TypeLits

import           Data.Foldable           (fold)
import           Data.Kind               (Type)
import           Data.Semigroup          (Semigroup (..))
import qualified Data.Sequence           as Seq
import           Data.Singletons         (SingI (..), SingKind (..))
import           Data.Singletons.Prelude (Sing (..))
import           Data.String             (fromString)
import qualified Data.Text               as Text
import           Data.Traversable        (for)
import           Data.Type.Equality      (type (==))

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Expression

data Label (n :: Symbol) = Label

instance n ~ t => IsLabel n (Label t) where
    fromLabel = Label

data ColumnExpression i where
    ColumnExpression :: Name -> Expression i a -> ColumnExpression i

data Columns i a
    = Columns (Seq.Seq (ColumnExpression i))
    | AllColumns (Expression i a)

buildColumns :: Columns i a -> Builder i Code
buildColumns = \case
    Columns cols -> do
        segments <-
            for cols $ \ (ColumnExpression name exp) -> do
                code <- buildExpression exp
                pure (code <> " AS " <> quoteName name)

        pure (fold (Seq.intersperse ", " segments))

    AllColumns exp ->
        buildExpression (Access exp "*")

data Column = Column Symbol Type

data instance Sing (z :: Column) where
    SColumn :: Sing name -> Sing ('Column name typ)

instance KnownSymbol name => SingI ('Column name typ) where
    sing = SColumn sing

singColumnNames :: Expression i a -> Sing (zs :: [Column]) -> Seq.Seq (ColumnExpression i)
singColumnNames exp = \case
    SNil ->
        Seq.empty

    SCons (SColumn (fromSing -> name)) zs ->
        ColumnExpression (Name name) (Access exp (Name name))
        Seq.<| singColumnNames exp zs

type family ColumnsOf a :: [Column]

type Selectable a =
    ( (ColumnsOf a == '[]) ~ 'False
    , SingI (ColumnsOf a)
    )

toColumns :: forall a i. Selectable a => Expression i a -> Columns i a
toColumns exp = Columns (singColumnNames exp (sing :: Sing (ColumnsOf a)))

allColumns :: Expression i a -> Columns i a
allColumns = AllColumns

data Field name a

singleton :: forall name i a. KnownSymbol name => Expression i a -> Columns i (Field name a)
singleton exp =
    Columns $ Seq.singleton $
        ColumnExpression (fromString (symbolVal (Label :: Label name))) exp

type instance ColumnsOf (Field name a) = '[ 'Column name a ]

instance KnownSymbol name => HasColumn name a (Field name a)

class HasColumn (n :: Symbol) t a | n a -> t where
    accessColumn :: Label n -> Expression i a -> Expression i t

    default accessColumn
        :: KnownSymbol n
        => Label n
        -> Expression i a
        -> Expression i t
    accessColumn proxy exp =
        Access exp (Name (Text.pack (symbolVal proxy)))

(!) :: HasColumn n t a => Expression i a -> Label n -> Expression i t
(!) = flip accessColumn

infixl 9 !
