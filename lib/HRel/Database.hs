{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HRel.Database where

import Control.Applicative  ((<|>))
import Control.Monad.Except

import           Data.ByteString    (ByteString)
import           Data.Coerce
import           Data.Foldable
import           Data.Maybe         (fromMaybe)
import           Data.String        (IsString (fromString))
import qualified Data.Text.Encoding as Text

import qualified Database.PostgreSQL.LibPQ as LibPQ

import System.Environment

import qualified Data.ByteString.Char8 as ByteString

import HRel.Control.Exception

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Select
import HRel.Database.Value

type Database = LibPQ.Connection

data ConnectionException
    = BadConnectionStatus LibPQ.ConnStatus
    deriving (Show, Eq)

instance Exception ConnectionException

-- | Connect to the PostgreSQL database using the given connection string.
connectDatabase :: Throws ConnectionException => Maybe ByteString -> IO Database
connectDatabase info = do
    envInfo <- lookupEnv "PGINFO"
    conn <- LibPQ.connectdb (fromMaybe mempty (info <|> fromString <$> envInfo))

    LibPQ.status conn >>= \case
        LibPQ.ConnectionOk -> pure conn
        status             -> throw (BadConnectionStatus status)

-- | Error that occurs during querying
data QueryException
    = NoResult
    | BadResponse ByteString
    | FatalError ByteString
    deriving (Show, Eq)

instance Exception QueryException

data Query i a = Query
    { queryCode   :: ByteString
    , queryParams :: i -> [Value]
    }

instance Show (Query i a) where
    show = ByteString.unpack . queryCode

toQuery :: Select i a -> Query i a
toQuery select =
    Query
        { queryCode   = Text.encodeUtf8 (unCode code)
        , queryParams = sequenceA params
        }
    where
        (params, code) = runBuilder (buildSelect select)

newtype Result a = Result LibPQ.Result

-- | Execute a query.
runQuery
    :: (MonadIO m, MonadThrow m, Throws QueryException)
    => Database
    -> Query i a
    -> i
    -> m (Result a)
runQuery database query input = do
    mbResult <- liftIO $
        LibPQ.execParams
            database
            (queryCode query)
            (coerce (queryParams query input))
            LibPQ.Text
    result <- maybe (throw NoResult) pure mbResult

    liftIO (LibPQ.resultStatus result) >>= \case
        LibPQ.BadResponse -> raiseError result BadResponse
        LibPQ.FatalError  -> raiseError result FatalError
        _                 -> pure (Result result)
    where
        raiseError result constructor = do
            mbErrorMessage <- liftIO (LibPQ.resultErrorMessage result)
            throw (constructor (fromMaybe mempty mbErrorMessage))

debugResult :: Result a -> IO ()
debugResult (Result result) = do
    rows <- LibPQ.ntuples result
    cols <- LibPQ.nfields result

    for_ [0 .. pred rows] $ \ row -> do
        putStrLn "---"
        for_ [0 .. pred cols] $ \ col -> do
            value <- LibPQ.getvalue result row col
            print value
