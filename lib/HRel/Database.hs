{-# LANGUAGE OverloadedStrings #-}

module HRel.Database (
	-- * Connection
	withDatabase,

	-- * Actions
	Action,
	runAction,

	query,
	query_,
	query1,
	query1_,
	execute,
	executeMany,
	execute_,
	insert,

	-- * Re-export stuff
	M.In (..),
	M.Only (..),
	M.Query,
	Database
) where

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

import           Data.Int
import           Data.Word
import           Data.Maybe
import           Data.Typeable
import qualified Data.ByteString.Char8              as BC

import qualified Database.MySQL.Base.Types          as M
import qualified Database.MySQL.Simple              as M
import qualified Database.MySQL.Simple.Param        as M
import qualified Database.MySQL.Simple.QueryParams  as M
import qualified Database.MySQL.Simple.QueryResults as M
import qualified Database.MySQL.Simple.Result       as M

import           Network.URI                        hiding (query)

-- | "TypeRef" for "URI"
typeOfURI :: TypeRep
typeOfURI =
	typeOf (undefined :: URI)

instance M.Result URI where
	convert field Nothing =
		throw (M.UnexpectedNull
		           (show (M.fieldType field))
		           (show typeOfURI)
		           "UnexpectedNull")
	convert field (Just value)
		| M.fieldType field `elem` [M.VarChar, M.VarString, M.String] =
			case parseURI (BC.unpack value) of
				Just uri -> uri
				Nothing  -> throw (M.ConversionFailed
				                       (show (M.fieldType field))
				                       (show typeOfURI)
				                       "UnexpectedNull")
		| otherwise =
			throw (M.Incompatible
			           (show (M.fieldType field))
			           (show typeOfURI)
			           "UnexpectedNull")

instance M.Param URI where
	render = M.render . show

-- | Connection to a MySQL server
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
type Action = ReaderT Database (MaybeT IO)

-- | Perform the given actions.
runAction :: (MonadIO m) => Database -> Action a -> m (Maybe a)
runAction db a =
	liftIO (handle noteException (runMaybeT (runReaderT a db)))
	where
		noteException (SomeException e) = do
			putStrLn ("runAction: " ++ show e)
			pure Nothing

-- |
liftAction :: IO (Maybe a) -> Action a
liftAction = ReaderT . const . MaybeT

-- | Query the database.
query :: (M.QueryParams p, M.QueryResults r) => M.Query -> p -> Action [r]
query q p = do
	con <- ask
	liftIO (M.query con q p)

-- | Query the database without parameters.
query_ :: (M.QueryResults r) => M.Query -> Action [r]
query_ q = do
	con <- ask
	liftIO (M.query_ con q)

-- | Same as "query" but returns only the first result.
query1 :: (M.QueryParams p, M.QueryResults r) => M.Query -> p -> Action r
query1 q p = do
	con <- ask
	liftAction (fmap listToMaybe (M.query con q p))

-- | Same as "query_" but returns only the first result.
query1_ :: (M.QueryResults r) => M.Query -> Action r
query1_ q = do
	con <- ask
	liftAction (fmap listToMaybe (M.query_ con q))

-- | Execute a statement and return the number of rows affected.
execute :: (M.QueryParams p) => M.Query -> p -> Action Int64
execute q p = do
	con <- ask
	liftIO (M.execute con q p)

-- | Execute many statements and return the number of rows affected.
executeMany :: (M.QueryParams p) => M.Query -> [p] -> Action Int64
executeMany q p = do
	con <- ask
	liftIO (M.executeMany con q p)

-- | Same as "execute" but does not expect a query parameter.
execute_ :: M.Query -> Action Int64
execute_ q = do
	con <- ask
	liftIO (M.execute_ con q)

-- | Execute a statement and return the last inserted ID.
insert :: (M.QueryParams p) => M.Query -> p -> Action Word64
insert q p = do
	con <- ask
	liftAction $ do
		num <- M.execute con q p
		if num >= 0 then
			Just <$> M.insertID con
		else
			pure Nothing
