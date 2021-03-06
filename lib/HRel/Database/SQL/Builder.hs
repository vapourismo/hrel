{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StrictData                 #-}

module HRel.Database.SQL.Builder
    ( -- * Builder
      Builder
    , runBuilder
    , evalBuilder
    , Name (..)
    , mkName
    , mkParam

      -- * Code
    , Code (..)
    , quoteString
    , quoteName
    )
where

import Control.Monad.State.Strict

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set           as Set
import           Data.String
import qualified Data.Text          as Text

import HRel.Database.Value

data BuilderState i =
    BuilderState
        { bsNames  :: Set.Set Text.Text
        , bsParams :: IntMap.IntMap (i -> Value)
        }

newtype Builder i a =
    Builder (State (BuilderState i) a)
    deriving (Functor, Applicative, Monad)

runBuilder :: Builder i a -> ([i -> Value], a)
runBuilder (Builder action) =
    (IntMap.elems params, x)
    where
        (x, BuilderState _ params) =
            runState action (BuilderState Set.empty IntMap.empty)

evalBuilder :: Builder i a -> a
evalBuilder (Builder action) =
    evalState action (BuilderState Set.empty IntMap.empty)

newtype Name = Name {unName :: Text.Text}
    deriving (Show, Eq, Ord, IsString)

mkName :: Text.Text -> Builder i Name
mkName prefix =
    Builder $ state $ \ state ->
        pickName state (prefix : map (buildName prefix) [(0 :: Word) ..])
    where
        buildName prefix index =
            Text.append prefix (Text.pack (show index))

        registerName state name =
            ( Name name
            , state {bsNames = Set.insert name (bsNames state)}
            )

        pickName _     []                     = error "This can't happen"
        pickName state (name : names)
            | Set.member name (bsNames state) = pickName state names
            | otherwise                       = registerName state name

newtype Code = Code {unCode :: Text.Text}
    deriving (Show, Eq, Ord, IsString, Semigroup, Monoid)

mkParam :: (i -> Value) -> Builder i Code
mkParam accessor =
    Builder $ state $ \ state ->
        let newIndex  = length (bsParams state)
            newParams = IntMap.insert newIndex accessor (bsParams state)
        in
            ( Code (Text.pack ('$' : show (1 + newIndex)))
            , state {bsParams = newParams}
            )

quoteString :: Char -> Text.Text -> Code
quoteString delim inner =
    Code $ Text.cons delim $
        Text.snoc
            (Text.replace single (Text.pack [delim, delim]) inner)
            delim
    where
        single = Text.singleton delim

quoteName :: Name -> Code
quoteName (Name name)
    | Text.any (== '"') name = quoteString '"' name
    | otherwise              = fromString (Text.unpack name)
