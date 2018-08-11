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
    )
where

import Prelude hiding ((>=))

import Control.Applicative

import           Data.String (IsString (..))
import qualified Data.Text   as Text

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Columns
import HRel.Database.SQL.Expression
import HRel.Database.SQL.Types

data Select i a where
    TableOnly
        :: Name
        -> Select i a

    Select
        :: (Expression i a -> Columns i)
        -> Select i a
        -> (Expression i a -> Expression i Bool)
        -> Maybe Word
        -> Maybe Word
        -> Select i b

instance IsString (Select i a) where
    fromString = TableOnly . fromString

instance Show (Select i a) where
    show = Text.unpack . unCode . evalBuilder . buildSelect

buildWhereClause :: Expression i Bool -> Builder i Code
buildWhereClause = \case
    BoolLit True -> pure ""
    whereClause  -> (" WHERE " <>) <$> buildExpression whereClause

buildSelect :: Select i a -> Builder i Code
buildSelect = \case
    TableOnly name ->
        pure ("TABLE " <> quoteName name)

    Select selectClause fromClause whereClause mbLimit mbOffset -> do
        bindName <- mkName "S"

        fromCode <-
            case fromClause of
                TableOnly name -> pure (quoteName name)
                _              -> (\ code -> "(" <> code <> ")") <$> buildSelect fromClause

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

table :: forall a i. Name -> Select i a
table = TableOnly

project :: (Expression i a -> Columns i) -> Select i a -> Select i b
project selector statement =
    Select selector statement (const true) Nothing Nothing

restrict :: (Expression i a -> Expression i Bool) -> Select i a -> Select i a
restrict restrictor statement =
    Select expand statement restrictor Nothing Nothing

limit :: Word -> Select i a -> Select i a
limit num = \case
    Select projector source restrictor limit offset ->
        Select projector source restrictor ((min num <$> limit) <|> Just num) offset

    source ->
        Select expand source (const true) (Just num) Nothing

offset :: Word -> Select i a -> Select i a
offset num = \case
    Select projector source restrictor limit offset ->
        Select projector source restrictor limit ((min num <$> offset) <|> Just num)

    source ->
        Select expand source (const true) Nothing (Just num)
