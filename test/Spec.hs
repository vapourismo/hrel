module Main (main) where

import qualified HRel.Control.Effect.EnvironmentSpec
import qualified HRel.Database.SQL.SelectSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    HRel.Control.Effect.EnvironmentSpec.spec
    HRel.Database.SQL.SelectSpec.spec
