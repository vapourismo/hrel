{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module HRel.Database.SQL.Builder
    ( Builder
    , runBuilder
    , mkName
    , quoteString
    , quoteName
    )
where

import Control.Monad.State

import qualified Data.Set    as Set
import           Data.String
import qualified Data.Text   as Text

import HRel.Database.SQL.Types

newtype Builder a =
    Builder (State (Set.Set Text.Text) a)
    deriving (Functor, Applicative, Monad)

runBuilder :: Builder a -> a
runBuilder (Builder action) =
    evalState action Set.empty

mkName :: Text.Text -> Builder Name
mkName prefix =
    Builder $ state $ \ state ->
        if Set.member prefix state then
            pickName state (map (buildName prefix) [(0 :: Word) ..])
        else
            registerName state prefix
    where
        buildName prefix index =
            Text.append prefix (Text.pack (show index))

        registerName state name =
            ( Name name
            , Set.insert name state )

        pickName _     []           = error "This can't happen"
        pickName state (name : names)
            | Set.member name state = pickName state names
            | otherwise             = registerName state name

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
