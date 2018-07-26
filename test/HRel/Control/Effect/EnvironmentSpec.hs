{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HRel.Control.Effect.EnvironmentSpec where

import Control.Monad.Reader (runReader, runReaderT)

import Test.Hspec
import Test.QuickCheck

import HRel.Control.Effect.Environment

type ExampleInput e = (Arbitrary e, Show e, Eq e)

testAsk
    :: ( ExampleInput e
       , HasEnvironment e f
       )
    => (forall a. f a -> e -> a)
    -> Property
testAsk run =
    property (\ e -> run ask e == e)

testNestedAsk
    :: ( ExampleInput e
       , HasEnvironment e f
       , ExampleInput s
       , HasEnvironment s f
       )
    => (forall a. f a -> e -> s -> a)
    -> Property
testNestedAsk run =
    property (\ e s -> run ask e s == e && run ask e s == s)

testLocal
    :: forall e f
    .  ( ExampleInput e
       , Function e
       , CoArbitrary e
       , HasEnvironment e f
       )
    => (forall a. f a -> e -> a)
    -> Property
testLocal run =
    property (\ (Fn f :: Fun e e) e -> run (local f ask) e == f e)

testNestedLocal
    :: forall e s f
    .  ( ExampleInput e
       , Function e
       , CoArbitrary e
       , HasEnvironment e f
       , ExampleInput s
       , Function s
       , CoArbitrary s
       , HasEnvironment s f
       , Applicative f
       )
    => (forall a. f a -> e -> s -> a)
    -> Property
testNestedLocal run =
    property $ \ (Fn f :: Fun e e) (Fn g :: Fun s s) e s ->
        run (local f ask) e s == f e
        && run (local g ask) e s == g s
        && run (local f (local g ((,) <$> ask <*> ask))) e s == (f e, g s)
        && run (local g (local f ((,) <$> ask <*> ask))) e s == (f e, g s)

spec :: Spec
spec =
    describe "HasEnvironment" $
        describe "ReaderT" $ do
            it "has correct 'ask' implementation" $
                testAsk (runReader @Int)
                .&&. testAsk (runReader @String)

            it "has correct 'ask' implementation for nested scenarios" $
                testNestedAsk (\ m e s -> runReader @String (runReaderT @Int m e) s)
                .&&. testNestedAsk (\ m e s -> runReader @Int (runReaderT @String m e) s)

            it "has correct 'local' implementation" $
                testLocal (runReader @Int)
                .&&. testLocal (runReader @Int)

            it "has correct 'local' implementation for nested scenarios" $
                testNestedLocal (\ m -> runReader @String . runReaderT @Int m)
                .&&. testNestedLocal (\ m -> runReader @Int . runReaderT @String m)
