{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module HRel.Application.Test (main) where

import Prelude hiding ((<=), (==), (>=))

import Data.Tagged
import Data.Text   (Text)

import HRel.Control.Exception

import HRel.Database
import HRel.Database.SQL.Columns
import HRel.Database.SQL.Expression
import HRel.Database.SQL.Select
import HRel.Database.Value

data TestTable

type instance ColumnsOf TestTable =
    '[ "x" ::: Int
     , "y" ::: Text
     ]

exampleQuery :: Query Int (Tagged "y" Text)
exampleQuery =
    toQuery
    $ project
        (\ row -> singleColumn @"y" (row ! #left ! #y))
    $ limit 1
    $ innerJoin
        (\ lhs rhs -> lhs ! #y == rhs ! #y)
        leftQuery
        rightQuery

    where
        leftQuery =
            project
                (\ row -> singleColumn @"y" (row ! #y))
            $ restrict
                (\ row -> row ! #x >= paramWith toValue)
                (table @TestTable "test_table")

        rightQuery =
            project
                (\ row -> singleColumn @"y" (row ! #y))
            $ restrict
                (\ row -> row ! #x <= paramWith toValue)
                (table @TestTable "test_table")

main :: IO ()
main = do
    db <- failOnException @ConnectionException $
        connectDatabase (Just "host=localhost dbname=hrel user=hrel")

    print exampleQuery

    result <- failOnException @QueryException $
        runQuery db exampleQuery 2

    debugResult result
