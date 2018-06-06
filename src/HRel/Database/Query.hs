{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

module HRel.Database.Query
    ( QueryRecipe (..)

    , Query (..)
    , toQuery

    , PrepQuery (..)
    , toPreparedQuery )
where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category
import Control.Monad.State

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as Char8String
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Profunctor            (Profunctor (..))
import           Data.Semigroup             (Semigroup (..))
import           Data.String                (IsString (fromString))

import HRel.Database.Types

-- | Recipe to construct a 'Query' or 'PrepQuery'
data QueryRecipe i p
    = Text ByteString
    | Hole (i -> p)
    | Branch (QueryRecipe i p) (QueryRecipe i p)
    deriving Functor

instance Profunctor QueryRecipe where
    dimap _    _     (Text text)      = Text text
    dimap left right (Hole func)      = Hole (right . func . left)
    dimap left right (Branch lhs rhs) = Branch (dimap left right lhs) (dimap left right rhs)

    {-# INLINE dimap #-}

instance Category QueryRecipe where
    id = Hole id

    {-# INLINE id #-}

    _              . Text text      = Text text
    other          . Branch lhs rhs = Branch (other . lhs) (other . rhs)
    Text text      . _              = Text text
    Hole lhs       . Hole rhs       = Hole (lhs . rhs)
    Branch lhs rhs . Hole func      = Branch (lmap func lhs) (lmap func rhs)

instance Arrow QueryRecipe where
    arr = Hole

    {-# INLINE arr #-}

    first (Text text)      = Text text
    first (Hole func)      = Hole (\ (x, y) -> (func x, y))
    first (Branch lhs rhs) = Branch (first lhs) (first rhs)

instance IsString (QueryRecipe i p) where
    fromString = Text . fromString

    {-# INLINE fromString #-}

instance Show (QueryRecipe i p) where
    show (Text text)      = Char8String.unpack text
    show (Hole _)         = "?"
    show (Branch lhs rhs) = show lhs ++ show rhs

instance Semigroup (QueryRecipe i p)

instance Monoid (QueryRecipe i p) where
    mempty = Text mempty

    {-# INLINE mempty #-}

    mappend (Text lhs) (Text rhs) = Text (mappend lhs rhs)
    mappend lhs rhs               = Branch lhs rhs

-- | Query
data Query i =
    Query
        { queryCode   :: ByteString
        , queryParams :: [i -> Value] }

instance Contravariant Query where
    contramap f query = query {queryParams = map (. f) (queryParams query)}

-- | Turn a recipe into a 'Query'.
toQuery :: QueryRecipe i Value -> Query i
toQuery query =
    Query code params
    where
        (_, code, params) = execState (unpackQuery query) (1 :: Word, mempty, [])

        insertCode prefix (index, code, params) = (index, prefix <> code, params)

        insertHole func (index, code, params) =
            ( index + 1
            , Char8String.pack ('$' : show index) <> code
            , func : params )

        unpackQuery (Text text)      = modify (insertCode text)
        unpackQuery (Hole func)      = modify (insertHole func)
        unpackQuery (Branch lhs rhs) = unpackQuery rhs >> unpackQuery lhs

-- | Preparable query
data PrepQuery i =
    PrepQuery
        { preparedQueryName :: ByteString
        , preparedQuery     :: Query i }

instance Contravariant PrepQuery where
    contramap f query = query {preparedQuery = contramap f (preparedQuery query)}

-- | Turn a recipe into a 'PrepQuery'.
toPreparedQuery :: ByteString -> QueryRecipe i Value -> PrepQuery i
toPreparedQuery name query =
    PrepQuery name (toQuery query)
