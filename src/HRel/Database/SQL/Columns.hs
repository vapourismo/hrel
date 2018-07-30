{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module HRel.Database.SQL.Columns
    ( Columns
    , singleton
    , buildColumns

    , Column (..)
    , ColumnsOf

    , Selectable
    , toColumns

    , HasColumn (..)
    , (!)

    , Label
    , mkLabel
    )
where

import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits

import           Data.Kind          (Type)
import           Data.List          (intersperse)
import           Data.Semigroup     ((<>))
import qualified Data.Text          as Text
import           Data.Type.Equality

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Expression
import HRel.Database.SQL.Types

data ColumnExpression where
    ColumnExpression :: Name -> Expression a -> ColumnExpression

type Columns = [ColumnExpression]

buildColumns :: Columns -> Query
buildColumns cols =
    mconcat $ intersperse ", " $
        map (\ (ColumnExpression name exp) -> buildExpression exp <> " AS " <> quoteName name)
            cols

singleton :: Name -> Expression a -> Columns
singleton name exp = [ColumnExpression name exp]

data Column = Column Symbol Type

class SelectableColumns xs where
    toSelectColumnExpressions :: Proxy# xs -> Expression a -> [ColumnExpression]

instance SelectableColumns '[] where
    toSelectColumnExpressions _ _ = []

instance (KnownSymbol n, SelectableColumns xs) => SelectableColumns ('Column n t ': xs) where
    toSelectColumnExpressions _ exp =
        ColumnExpression name (Access exp name)
        : toSelectColumnExpressions (proxy# :: Proxy# xs) exp
        where
            name = Name (Text.pack (symbolVal' (proxy# :: Proxy# n)))

type family ColumnsOf a :: [Column]

type Selectable a =
    ( (ColumnsOf a == '[]) ~ 'False
    , SelectableColumns (ColumnsOf a)
    )

toColumns :: forall a. Selectable a => Expression a -> Columns
toColumns = toSelectColumnExpressions (proxy# :: Proxy# (ColumnsOf a))

class HasColumn (n :: Symbol) t a | n a -> t where
    accessColumn :: Label n -> Expression a -> Expression t

    default accessColumn
        :: KnownSymbol n
        => Label n
        -> Expression a
        -> Expression t
    accessColumn proxy exp =
        Access exp (Name (Text.pack (symbolVal proxy)))

(!) :: HasColumn n t a => Expression a -> Label n -> Expression t
(!) = flip accessColumn

infixl 9 !

data Label (n :: Symbol)

mkLabel :: forall n. Label n
mkLabel = unsafeCoerce# (proxy# :: Proxy# n)

instance n ~ t => IsLabel n (Label t) where
    fromLabel = mkLabel
