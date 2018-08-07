{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module HRel.Database.SQL.Builder
    ( Param
    , Builder
    , runBuilder
    , evalBuilder
    , mkName
    , mkParam
    , quoteString
    , quoteName
    )
where

import Control.Monad.State

import           Data.Foldable      (toList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set           as Set
import           Data.String
import qualified Data.Text          as Text

import HRel.Database.SQL.Types

type Param = ()

data BuilderState i =
    BuilderState
        { bsNames  :: Set.Set Text.Text
        , bsParams :: IntMap.IntMap (i -> Param)
        }

newtype Builder i a =
    Builder (State (BuilderState i) a)
    deriving (Functor, Applicative, Monad)

runBuilder :: Builder i a -> ([i -> Param], a)
runBuilder (Builder action) =
    (toList params, x)
    where
        (x, BuilderState _ params) =
            runState action (BuilderState Set.empty IntMap.empty)

evalBuilder :: Builder i a -> a
evalBuilder (Builder action) =
    evalState action (BuilderState Set.empty IntMap.empty)

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

mkParam :: (i -> Param) -> Builder i Int
mkParam accessor =
    Builder $ state $ \ state ->
        let newIndex  = length (bsParams state)
            newParams = IntMap.insert newIndex accessor (bsParams state)
        in
            (newIndex, state {bsParams = newParams})

quoteString :: Char -> Text.Text -> Query
quoteString delim inner =
    fromString
        ( delim
        : Text.unpack (Text.replace single (Text.pack [delim, delim]) inner)
        ++ [delim]
        )
    where
        single = Text.singleton delim

quoteName :: Name -> Query
quoteName (Name name)
    | Text.any (== '"') name = quoteString '"' name
    | otherwise              = fromString (Text.unpack name)
