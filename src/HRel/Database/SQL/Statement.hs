{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}

module HRel.Database.SQL.Statement where

import Prelude hiding ((>=))

import qualified Data.ByteString.Char8 as CharString

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Columns
import HRel.Database.SQL.Expression
import HRel.Database.SQL.Types

data Statement i a where
    TableOnly
        :: Name
        -> Statement i a

    Select
        :: (Expression i a -> Columns i)
        -> Statement i a
        -> (Expression i a -> Expression i PgBool)
        -> Statement i b

instance Show (Statement i a) where
    show = CharString.unpack . fromQuery . evalBuilder . buildStatement

buildWhereClause :: Expression i PgBool -> Builder i Query
buildWhereClause = \case
    BoolLit True -> pure ""
    whereClause  -> (" WHERE " <>) <$> buildExpression whereClause

buildStatement :: Statement i a -> Builder i Query
buildStatement = \case
    TableOnly name ->
        pure ("TABLE " <> quoteName name)

    Select selectClause fromClause whereClause -> do
        bindName <- mkName "S"

        fromCode <-
            case fromClause of
                TableOnly name -> pure (quoteName name)
                _              -> (\ code -> "(" <> code <> ")") <$> buildStatement fromClause

        selectCode <- buildColumns (selectClause (Variable bindName))
        whereCode  <- buildWhereClause (whereClause (Variable bindName))

        pure $ mconcat
            [ "SELECT "
            , selectCode
            , " FROM "
            , fromCode
            , " AS "
            , quoteName bindName
            , whereCode
            ]

data TestTable

instance HasColumn "x" PgNumber TestTable
instance HasColumn "y" PgString TestTable

type instance ColumnsOf TestTable =
    '[ 'Column "x" PgNumber
     , 'Column "y" PgString
     ]

project :: (Expression i a -> Columns i) -> Statement i a -> Statement i b
project selector statement =
    Select selector statement (const true)

restrict
    :: Selectable a
    => (Expression i a -> Expression i PgBool)
    -> Statement i a
    -> Statement i a
restrict restrictor statement =
    Select toColumns statement restrictor

example :: Statement i PgNumber
example =
    project selectClause (restrict whereClause fromClause)
    where
        selectClause row =
            singleton "y" (row ! #y)

        whereClause row =
            row ! #x >= 0

        fromClause :: Statement i TestTable
        fromClause = TableOnly (Name "test_table")
