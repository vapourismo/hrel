{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Database
    ( Database
    , connectDatabase

    , QueryError (..)
    , runQuery

    , QueryRecipe (..)
    , Query (..)
    , PrepQuery (..)

    , Value
    , Marshal (..) )
where

import Prelude hiding (id, (.))

import Control.Applicative  ((<|>))
import Control.Monad.Except

import Data.ByteString (ByteString)
import Data.Maybe      (fromMaybe)
import Data.String     (IsString (fromString))

import qualified Database.PostgreSQL.LibPQ as LibPQ

import System.Environment

import HRel.Database.Marshal
import HRel.Database.Query
import HRel.Database.Types

type Database = LibPQ.Connection

-- | Connect to the PostgreSQL database using the given connection string.
connectDatabase :: Maybe ByteString -> IO Database
connectDatabase info = do
    envInfo <- lookupEnv "PGINFO"
    LibPQ.connectdb (fromMaybe mempty (info <|> fromString <$> envInfo))

-- | Error that occurs during querying
data QueryError
    = NoResult
    | BadResponse ByteString
    | FatalError ByteString
    deriving (Show, Eq)

-- | Execute a query.
runQuery
    :: (MonadIO m, MonadError QueryError m)
    => Database
    -> Query i
    -> i
    -> m LibPQ.Result
runQuery database query input = do
    mbResult <- liftIO $
        LibPQ.execParams
            database
            (queryCode query)
            (map (\ gen -> unValue (gen input)) (queryParams query))
            LibPQ.Text
    result <- maybe (throwError NoResult) pure mbResult

    liftIO (LibPQ.resultStatus result) >>= \case
        LibPQ.BadResponse -> raiseError result BadResponse
        LibPQ.FatalError  -> raiseError result FatalError
        _                 -> pure result
    where
        raiseError result constructor = do
            mbErrorMessage <- liftIO (LibPQ.resultErrorMessage result)
            throwError (constructor (fromMaybe mempty mbErrorMessage))
