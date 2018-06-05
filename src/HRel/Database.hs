{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Database
where

import Prelude hiding (id, (.))

import Control.Applicative ((<|>))
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.State

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as Char8String
import           Data.Functor.Contravariant (Contravariant (..))
import           Data.List                  (intersperse)
import           Data.Maybe                 (fromMaybe)
import           Data.Profunctor            (Profunctor (..))
import           Data.Semigroup             (Semigroup (..))
import           Data.String                (IsString (fromString))
import qualified Database.PostgreSQL.LibPQ  as LibPQ

import System.Environment

type Database = LibPQ.Connection

connectDatabase :: Maybe ByteString -> IO Database
connectDatabase info = do
    envInfo <- lookupEnv "PGINFO"
    LibPQ.connectdb (fromMaybe mempty (info <|> fromString <$> envInfo))

data Query i p
    = Text ByteString
    | Hole (i -> p)
    | Branch (Query i p) (Query i p)
    deriving Functor

instance Profunctor Query where
    dimap _    _     (Text text)      = Text text
    dimap left right (Hole func)      = Hole (right . func . left)
    dimap left right (Branch lhs rhs) = Branch (dimap left right lhs) (dimap left right rhs)

    {-# INLINE dimap #-}

instance Category Query where
    id = Hole id

    {-# INLINE id #-}

    _              . Text text      = Text text
    other          . Branch lhs rhs = Branch (other . lhs) (other . rhs)
    Text text      . _              = Text text
    Hole lhs       . Hole rhs       = Hole (lhs . rhs)
    Branch lhs rhs . Hole func      = Branch (lmap func lhs) (lmap func rhs)

instance Arrow Query where
    arr = Hole

    {-# INLINE arr #-}

    first (Text text)      = Text text
    first (Hole func)      = Hole (\ (x, y) -> (func x, y))
    first (Branch lhs rhs) = Branch (first lhs) (first rhs)

instance IsString (Query i p) where
    fromString = Text . fromString

    {-# INLINE fromString #-}

instance Show (Query i p) where
    show (Text text)      = Char8String.unpack text
    show (Hole _)         = "?"
    show (Branch lhs rhs) = show lhs ++ show rhs

instance Semigroup (Query i p)

instance Monoid (Query i p) where
    mempty = Text mempty

    {-# INLINE mempty #-}

    mappend (Text lhs) (Text rhs) = Text (mappend lhs rhs)
    mappend lhs rhs               = Branch lhs rhs

data PreparedQuery i =
    PreparedQuery
        { preparedQueryName       :: ByteString
        , preparedQueryCode       :: ByteString
        , preparedQueryParameters :: [i -> Value] }

instance Contravariant PreparedQuery where
    contramap f preparedQuery =
        preparedQuery
            { preparedQueryParameters = map (. f) (preparedQueryParameters preparedQuery) }

toPreparedQuery :: ByteString -> Query i Value -> PreparedQuery i
toPreparedQuery name query =
    PreparedQuery name code params
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

fromPreparedQuery :: PreparedQuery i -> Query i Value
fromPreparedQuery preparedQuery =
    mconcat
        [ "EXECUTE "
        , Text (preparedQueryName preparedQuery)
        , "("
        , mconcat (intersperse "," (map arr (preparedQueryParameters preparedQuery)))
        , ")" ]

newtype Value = Value {unValue :: Maybe (LibPQ.Oid, ByteString, LibPQ.Format)}

nullValue :: Value
nullValue = Value Nothing

{-# INLINE nullValue #-}

class Marshal a where
    marshal :: Query a Value

instance Marshal Value where
    marshal = id

    {-# INLINE marshal #-}

instance Marshal a => Marshal (Maybe a) where
    marshal =
        fixNothing <$> first marshal . arr (join (,))
        where
            fixNothing (_    , Nothing) = nullValue
            fixNothing (value, _      ) = value

    {-# INLINE marshal #-}

