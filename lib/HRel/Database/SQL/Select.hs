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

module HRel.Database.SQL.Select
    ( Select
    , buildSelect
    , table
    , project
    , restrict
    , limit
    , offset
    , innerJoin
    )
where

import Prelude hiding ((>=))

import Control.Applicative

import           Data.String (IsString (..))
import qualified Data.Text   as Text

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Columns
import HRel.Database.SQL.Expression

data Join a b

type instance ColumnsOf (Join a b) =
    '[ 'Column "left"  a
     , 'Column "right" b
     ]

instance HasColumn "left"  a (Join a b)
instance HasColumn "right" b (Join a b)

data Select i a where
    TableOnly
        :: Name
        -> Select i a

    Select
        :: (Expression i a -> Columns i b)       -- ^ SELECT clause
        -> Select i a                            -- ^ FROM clause
        -> (Expression i a -> Expression i Bool) -- ^ WHERE clause
        -> Maybe Word                            -- ^ LIMIT clause
        -> Maybe Word                            -- ^ OFFSET clause
        -> Select i b

    InnerJoin
        :: Select i a
        -> Select i b
        -> (Expression i a -> Expression i b -> Expression i Bool)
        -> Select i (Join a b)

instance IsString (Select i a) where
    fromString = TableOnly . fromString

instance Show (Select i a) where
    show = Text.unpack . unCode . evalBuilder . buildSelect

buildWhereClause :: Expression i Bool -> Builder i Code
buildWhereClause = \case
    BoolLit True -> pure ""
    whereClause  -> (" WHERE " <>) <$> buildExpression whereClause

buildFromCode :: Select i a -> Builder i Code
buildFromCode = \case
    TableOnly name -> pure (quoteName name)
    fromClause     -> (\ code -> "(" <> code <> ")") <$> buildSelect fromClause

buildSelect :: Select i a -> Builder i Code
buildSelect = \case
    TableOnly name ->
        pure ("TABLE " <> quoteName name)

    Select selectClause fromClause whereClause mbLimit mbOffset -> do
        bindName <- mkName "S"

        fromCode <- buildFromCode fromClause

        selectCode <- buildColumns (selectClause (Variable bindName))
        whereCode  <- buildWhereClause (whereClause (Variable bindName))

        let limitCode =
                case mbLimit of
                    Just num -> " LIMIT " <> fromString (show num)
                    _        -> ""

        let offsetCode =
                case mbOffset of
                    Just num -> " OFFSET " <> fromString (show num)
                    _        -> ""

        pure $ mconcat
            [ "SELECT "
            , selectCode
            , " FROM "
            , fromCode
            , " AS "
            , quoteName bindName
            , whereCode
            , limitCode
            , offsetCode
            ]

    InnerJoin leftSelect rightSelect condition -> do
        leftCode  <- buildFromCode leftSelect
        leftName  <- mkName "L"
        rightCode <- buildFromCode rightSelect
        rightName <- mkName "R"

        conditionCode <- buildExpression (condition (Variable leftName) (Variable rightName))

        pure $ mconcat
            [ "SELECT "
            , quoteName leftName
            , " AS left, "
            , quoteName rightName
            , " AS right FROM "
            , leftCode
            , " AS "
            , quoteName leftName
            , " INNER JOIN "
            , rightCode
            , " AS "
            , quoteName rightName
            , " ON ("
            , conditionCode
            , ")"
            ]


table :: forall a i. Name -> Select i a
table = TableOnly

project :: (Expression i a -> Columns i b) -> Select i a -> Select i b
project selector statement =
    Select selector statement (const true) Nothing Nothing

restrict :: (Expression i a -> Expression i Bool) -> Select i a -> Select i a
restrict restrictor statement =
    Select allColumns statement restrictor Nothing Nothing

limit :: Word -> Select i a -> Select i a
limit num = \case
    Select projector source restrictor limit offset ->
        Select projector source restrictor ((min num <$> limit) <|> Just num) offset

    source ->
        Select allColumns source (const true) (Just num) Nothing

offset :: Word -> Select i a -> Select i a
offset num = \case
    Select projector source restrictor limit offset ->
        Select projector source restrictor limit ((min num <$> offset) <|> Just num)

    source ->
        Select allColumns source (const true) Nothing (Just num)

innerJoin
    :: (Expression i a -> Expression i b -> Expression i Bool)
    -> Select i a
    -> Select i b
    -> Select i (Join a b)
innerJoin f lhs rhs =
    InnerJoin lhs rhs f

