{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module HRel.Database.SQL.Columns where

import GHC.OverloadedLabels
import GHC.TypeLits

import           Data.Foldable           (fold)
import           Data.Kind               (Type)
import           Data.List               (intersperse)
import           Data.Semigroup          (Semigroup (..))
import           Data.Singletons         (SingI (..), SingKind (..))
import           Data.Singletons.Prelude (Sing (..))
import           Data.String             (fromString)
import           Data.Tagged             (Tagged)
import qualified Data.Text               as Text
import           Data.Traversable        (for)

import HRel.Data.Type.List          (Find, NonEmpty)
import HRel.Database.SQL.Builder    (Builder, Code, Name (..), quoteName)
import HRel.Database.SQL.Expression (Expression (..), buildExpression)

----------------------------------------------------------------------------------------------------
-- Label

-- | Convenient 'Proxy'-like type that is easily constructible with @OverloadedLabels@
data Label (n :: Symbol) = Label

instance n ~ t => IsLabel n (Label t) where
    fromLabel = Label

----------------------------------------------------------------------------------------------------
-- Field data kind

-- | Column declaration with name and type
data Column = Column Symbol Type

data instance Sing (col :: Column) where
    SColumn :: Sing name -> Sing (name ::: typ)

instance KnownSymbol name => SingI (name ::: typ) where
    sing = SColumn sing

-- | Nicer alias for 'Column' constructor
type (:::) = 'Column

infix 0 :::

----------------------------------------------------------------------------------------------------
-- ColumnsOf type family

-- | Columns of a type
type family ColumnsOf a :: [Column]

type instance ColumnsOf (Tagged name a) = '[name ::: a]

----------------------------------------------------------------------------------------------------
-- Columns data type

data ColumnExpression i where
    ColumnExpression :: Name -> Expression i a -> ColumnExpression i

-- | Collection of columns
data Columns i a
    = Columns [ColumnExpression i]
    | AllColumns (Expression i a)

-- | Expand all columns of a type.
allColumns :: Expression i row -> Columns i row
allColumns = AllColumns

-- | Select a single expression in a single column.
singleColumn :: forall name a i. KnownSymbol name => Expression i a -> Columns i (Tagged name a)
singleColumn exp =
    Columns [ColumnExpression (fromString (symbolVal (Label :: Label name))) exp]

-- | Render columns as the body of a @SELECT@ statement.
buildColumns :: Columns i a -> Builder i Code
buildColumns = \case
    Columns cols -> do
        segments <-
            for cols $ \ (ColumnExpression name exp) -> do
                code <- buildExpression exp
                pure (code <> " AS " <> quoteName name)

        pure (fold (intersperse ", " segments))

    AllColumns exp ->
        buildExpression (Access exp "*")

----------------------------------------------------------------------------------------------------
-- Selection

-- | Type is selectable (i.e. has columns to expand)
type Selectable row =
    ( NonEmpty (ColumnsOf row)
    , SingI (ColumnsOf row)
    )

-- | Select an expression.
selectExpression :: Selectable row => Expression i row -> Columns i row
selectExpression (exp :: Expression i row) =
    Columns (singColumnNames (sing :: Sing (ColumnsOf row)))
    where
        singColumnNames :: Sing (cols :: [Column]) -> [ColumnExpression i]
        singColumnNames = \case
            SNil -> []

            SCons (SColumn (Name . fromSing -> name)) rest ->
                ColumnExpression name (Access exp name) : singColumnNames rest

----------------------------------------------------------------------------------------------------
-- Access columns

-- | @row@ has a column @name@ of type @typ@.
type HasColumn name typ row = (KnownSymbol name, Find name (ColumnsOf row) ~ typ)

-- | Project a column.
(!) :: HasColumn name typ row => Expression i row -> Label name -> Expression i typ
(!) subject fieldProxy =
    Access subject (Name (Text.pack (symbolVal fieldProxy)))

infixl 9 !
