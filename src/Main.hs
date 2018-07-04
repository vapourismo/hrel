{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude (IO, ($))

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

import HRel.Database.SQL.Expression

data Select a where
    Select :: Expression a -> Select a

selectToSql :: Select a -> Text.Text
selectToSql = \case
    Select expr -> Text.append "SELECT " (expressionToSql expr)

main :: IO ()
main = Text.putStrLn $ selectToSql $
    Select (abs (negate (Literal "1" :: Expression PgNumber)))
