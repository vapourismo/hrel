{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import HRel.Database

query :: QueryRecipe Int Value
query =
    mconcat
        [ "SELECT * FROM test_table WHERE x = "
        , marshal ]

main :: IO ()
main = print query
