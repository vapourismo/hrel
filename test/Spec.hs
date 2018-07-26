module Main (main) where

import qualified HRel.Control.Effect.EnvironmentSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    HRel.Control.Effect.EnvironmentSpec.spec
