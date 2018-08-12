{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module HRel.Application.Test (main) where

import Prelude hiding ((<=), (==), (>=))

import Data.Text (Text)

import HRel.Control.Exception

import HRel.Database
import HRel.Database.SQL.Columns
import HRel.Database.SQL.Expression
import HRel.Database.SQL.Select
import HRel.Database.Value

data TestTable

instance HasColumn "x" Int TestTable
instance HasColumn "y" Text TestTable

type instance ColumnsOf TestTable =
    '[ 'Column "x" Int
     , 'Column "y" Text
     ]

exampleQuery :: Query Int (Field "y" Text)
exampleQuery =
    toQuery
    $ project
        (\ row -> singleton @"y" (row ! #left ! #y))
    $ limit 1
    $ innerJoin
        (\ lhs rhs -> lhs == rhs)
        leftQuery
        rightQuery

    where
        leftQuery =
            project
                (\ row -> singleton @"y" (row ! #y))
            $ restrict
                (\ row -> row ! #x >= paramWith toValue)
                (table @TestTable "test_table")

        rightQuery =
            project
                (\ row -> singleton @"y" (row ! #y))
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
