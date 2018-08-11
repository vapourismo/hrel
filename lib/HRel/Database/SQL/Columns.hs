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
    , expand

    , HasColumn (..)
    , (!)

    , Label (..)
    )
where

import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits

import           Data.Kind          (Type)
import           Data.List          (intersperse)
import           Data.Semigroup     ((<>))
import qualified Data.Text          as Text
import           Data.Traversable   (for)
import           Data.Type.Equality

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Expression
import HRel.Database.SQL.Types

data Label (n :: Symbol) = Label

instance n ~ t => IsLabel n (Label t) where
    fromLabel = Label

data ColumnExpression i where
    ColumnExpression :: Maybe Name -> Expression i a -> ColumnExpression i

type Columns i = [ColumnExpression i]

buildColumns :: Columns i -> Builder i Query
buildColumns cols = do
    segments <-
        for cols $ \ (ColumnExpression mbName exp) -> do
            code <- buildExpression exp
            pure $ case mbName of
                Just name -> code <> " AS " <> quoteName name
                _         -> code

    pure (mconcat (intersperse ", " segments))

singleton :: Name -> Expression i a -> Columns i
singleton name exp = [ColumnExpression (Just name) exp]

data Column = Column Symbol Type

class SelectableColumns xs where
    toSelectColumnExpressions :: Proxy# xs -> Expression i a -> [ColumnExpression i]

instance SelectableColumns '[] where
    toSelectColumnExpressions _ _ = []

instance (KnownSymbol n, SelectableColumns xs) => SelectableColumns ('Column n t ': xs) where
    toSelectColumnExpressions _ exp =
        ColumnExpression (Just name) (Access exp name)
        : toSelectColumnExpressions (proxy# :: Proxy# xs) exp
        where
            name = Name (Text.pack (symbolVal' (proxy# :: Proxy# n)))

type family ColumnsOf a :: [Column]

type Selectable a =
    ( (ColumnsOf a == '[]) ~ 'False
    , SelectableColumns (ColumnsOf a)
    )

toColumns :: forall a i. Selectable a => Expression i a -> Columns i
toColumns = toSelectColumnExpressions (proxy# :: Proxy# (ColumnsOf a))

expand :: Expression i a -> Columns i
expand exp =
    [ColumnExpression Nothing (Access exp "*")]

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
