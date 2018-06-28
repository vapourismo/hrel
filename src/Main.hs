{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import GHC.Records
import GHC.TypeLits

import Control.Arrow

import HRel.Database

field
    :: forall (name :: Symbol) typ rec
    .  (HasField name rec typ, Marshal typ)
    => QueryRecipe rec Value
field = arr (getField @name @rec @typ) >>> marshal

query :: HasField "x" a Int => QueryRecipe a Value
query =
    mconcat
        [ "SELECT * FROM test_table WHERE x = "
        , field @"x" @Int ]

data A = A {x :: Int, y :: String}

main :: IO ()
main = print (query @A)
