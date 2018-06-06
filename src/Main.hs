{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import HRel.Database

query :: QueryRecipe Value Value
query =
    mconcat
        [ "SELECT "
        , marshal
        , ", * FROM my_table WHERE value > "
        , marshal ]

main :: IO ()
main = print query
