{-# LANGUAGE OverloadedStrings #-}

module HRel.Database (
	-- * Connection
	withDatabase,

	-- * Actions
	Action,
	runAction,

	query,
	query_,
	execute,
	execute_,
	insert,

	-- * Re-export stuff
	M.In (..),
	M.Only (..),
	Database
) where

import           Control.Exception
import           Control.Monad.Reader

import           Data.Int
import           Data.Word

import qualified Database.MySQL.Base.Types          as M
import qualified Database.MySQL.Simple              as M
import qualified Database.MySQL.Simple.QueryParams  as M
import qualified Database.MySQL.Simple.QueryResults as M

type Database = M.Connection

-- | Create a scope for the database connection.
withDatabase :: (Database -> IO a) -> IO a
withDatabase =
	bracket connectDatabase M.close
	where
		connectDatabase =
			M.connect (M.defaultConnectInfo {
				M.connectHost     = "localhost",
				M.connectDatabase = "hrelwa03",
				M.connectUser     = "root", -- Change later
				M.connectOptions  = [M.Reconnect True]
			})

-- | Database action
type Action = ReaderT Database IO

-- | Perform the given actions.
runAction :: (MonadIO m) => Database -> Action a -> m a
runAction db a =
	liftIO (runReaderT a db)

-- | Query the database.
query :: (M.QueryParams p, M.QueryResults r) => M.Query -> p -> Action [r]
query q p = do
	con <- ask
	lift (M.query con q p)

-- | Query the database without parameters.
query_ :: (M.QueryResults r) => M.Query -> Action [r]
query_ q = do
	con <- ask
	lift (M.query_ con q)

-- | Execute a statement and return the number of rows affected.
execute :: (M.QueryParams p) => M.Query -> p -> Action Int64
execute q p = do
	con <- ask
	lift (M.execute con q p)

-- | Same as "execute" but does not expect a query parameter.
execute_ :: M.Query -> Action Int64
execute_ q = do
	con <- ask
	lift (M.execute_ con q)

-- |
insert :: (M.QueryParams p) => M.Query -> p -> Action (Maybe Word64)
insert q p = do
	num <- execute q p
	if num >= 0 then
		Just <$> (ask >>= liftIO . M.insertID)
	else
		pure Nothing
