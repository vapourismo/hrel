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

module HRel.Database.SQL.Statement
    ( Statement
    , buildStatement
    , table
    , project
    , restrict
    , limit
    , offset
    )
where

import Prelude hiding ((>=))

import Control.Applicative

import qualified Data.ByteString.Char8 as CharString
import           Data.String           (IsString (..))

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
        -> (Expression i a -> Expression i Bool)
        -> Maybe Word
        -> Maybe Word
        -> Statement i b

instance IsString (Statement i a) where
    fromString = TableOnly . fromString

instance Show (Statement i a) where
    show = CharString.unpack . fromQuery . evalBuilder . buildStatement

buildWhereClause :: Expression i Bool -> Builder i Query
buildWhereClause = \case
    BoolLit True -> pure ""
    whereClause  -> (" WHERE " <>) <$> buildExpression whereClause

buildStatement :: Statement i a -> Builder i Query
buildStatement = \case
    TableOnly name ->
        pure ("TABLE " <> quoteName name)

    Select selectClause fromClause whereClause mbLimit mbOffset -> do
        bindName <- mkName "S"

        fromCode <-
            case fromClause of
                TableOnly name -> pure (quoteName name)
                _              -> (\ code -> "(" <> code <> ")") <$> buildStatement fromClause

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

table :: forall a i. Name -> Statement i a
table = TableOnly

project :: (Expression i a -> Columns i) -> Statement i a -> Statement i b
project selector statement =
    Select selector statement (const true) Nothing Nothing

restrict :: (Expression i a -> Expression i Bool) -> Statement i a -> Statement i a
restrict restrictor statement =
    Select expand statement restrictor Nothing Nothing

limit :: Word -> Statement i a -> Statement i a
limit num = \case
    Select projector source restrictor limit offset ->
        Select projector source restrictor ((min num <$> limit) <|> Just num) offset

    source ->
        Select expand source (const true) (Just num) Nothing

offset :: Word -> Statement i a -> Statement i a
offset num = \case
    Select projector source restrictor limit offset ->
        Select projector source restrictor limit ((min num <$> offset) <|> Just num)

    source ->
        Select expand source (const true) Nothing (Just num)
