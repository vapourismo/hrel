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

module HRel.Database.SQL.Columns where

import GHC.OverloadedLabels
import GHC.TypeLits

import           Data.Foldable           (fold)
import           Data.Kind               (Type)
import           Data.Semigroup          (Semigroup (..))
import qualified Data.Sequence           as Seq
import           Data.Singletons         (SingI (..), SingKind (..))
import           Data.Singletons.Prelude (Sing (..))
import           Data.String             (fromString)
import           Data.Tagged             (Tagged)
import qualified Data.Text               as Text
import           Data.Traversable        (for)
import           Data.Type.Equality      (type (==))

import HRel.Data.Type.List
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

type (:::) = 'Column

infix 0 :::

data instance Sing (z :: Column) where
    SColumn :: Sing name -> Sing (name ::: typ)

instance KnownSymbol name => SingI (name ::: typ) where
    sing = SColumn sing

type family ColumnsOf a :: [Column]

type instance ColumnsOf (Tagged name a) = '[name ::: a]

type NonEmpty xs = (xs == '[]) ~ 'False

type Selectable a =
    ( NonEmpty (ColumnsOf a)
    , SingI (ColumnsOf a)
    )

toColumns :: forall a i. Selectable a => Expression i a -> Columns i a
toColumns exp =
    Columns (singColumnNames (sing :: Sing (ColumnsOf a)))
    where
        singColumnNames :: Sing (cols :: [Column]) -> Seq.Seq (ColumnExpression i)
        singColumnNames = \case
            SNil ->
                Seq.empty

            SCons (SColumn (Name . fromSing -> name)) rest ->
                ColumnExpression name (Access exp name)
                Seq.<| singColumnNames rest


allColumns :: Expression i a -> Columns i a
allColumns = AllColumns

singleColumn :: forall name a i. KnownSymbol name => Expression i a -> Columns i (Tagged name a)
singleColumn =
    Columns
    . Seq.singleton
    . ColumnExpression (fromString (symbolVal (Label :: Label name)))

type HasColumn n t a = (KnownSymbol n, Find n (ColumnsOf a) ~ t)

(!) :: HasColumn n t a => Expression i a -> Label n -> Expression i t
(!) subject fieldProxy =
    Access subject (Name (Text.pack (symbolVal fieldProxy)))

infixl 9 !
