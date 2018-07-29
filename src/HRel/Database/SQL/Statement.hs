{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HRel.Database.SQL.Statement where

import Prelude hiding ((>=))

import Data.Monoid

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Columns
import HRel.Database.SQL.Expression
import HRel.Database.SQL.Types

data Statement a where
    TableOnly
        :: Name
        -> Statement a

    Select
        :: (Expression a -> Columns)
        -> Statement a
        -> (Expression a -> Expression PgBool)
        -> Statement b

buildWhereClause :: Expression PgBool -> Query
buildWhereClause = \case
    BoolLit True -> ""
    whereClause  -> " WHERE " <> buildExpression whereClause

buildStatement :: Statement a -> Builder Query
buildStatement = \case
    TableOnly name ->
        pure ("TABLE " <> quoteName name)

    Select selectClause fromClause whereClause -> do
        bindName <- mkName "S"

        fromCode <-
            case fromClause of
                TableOnly name -> pure (quoteName name)
                _              -> (\ code -> "(" <> code <> ")") <$> buildStatement fromClause

        pure $ mconcat
            [ "SELECT "
            , buildColumns (selectClause (Variable bindName))
            , " FROM "
            , fromCode
            , " AS "
            , quoteName bindName
            , buildWhereClause (whereClause (Variable bindName))
            ]

data TestTable

instance HasColumn "x" PgNumber TestTable
instance HasColumn "y" PgString TestTable

type instance ColumnsOf TestTable =
    '[ 'Column "x" PgNumber
     , 'Column "y" PgString
     ]

project :: (Expression a -> Columns) -> Statement a -> Statement b
project selector statement =
    Select selector statement (const true)

restrict :: Selectable a => (Expression a -> Expression PgBool) -> Statement a -> Statement a
restrict restrictor statement =
    Select toColumns statement restrictor

example :: Query
example =
    runBuilder $ buildStatement $
        project selectClause (restrict whereClause fromClause)
    where
        selectClause row =
            singleton "y" (row #> #y)

        whereClause row =
            row #> #x >= 0

        fromClause :: Statement TestTable
        fromClause = TableOnly (Name "test_table")
