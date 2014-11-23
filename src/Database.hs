{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

import Data.Word
import Data.Typeable
import Data.SafeCopy
import Data.Acid
import Data.Acid.Remote
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M

import Network
import Network.URI

import Control.Concurrent
import Control.Concurrent.Thread.Delay
import Control.Exception
import Control.Monad.State
import Control.Monad.Reader

type Group = (,) [B.ByteString] [URI]
data Database = Database { dbStorage :: M.Map Word64 Group
                         , dbCounter :: Word64 }
	deriving (Show, Typeable)

deriveSafeCopy 0 'base ''URIAuth
deriveSafeCopy 0 'base ''URI
deriveSafeCopy 0 'base ''Database

insertGroup :: Group -> Update Database Word64
insertGroup grp = do
	Database m i <- get
	put (Database (M.insert i grp m) (i + 1))
	return i

queryGroups :: Query Database [Group]
queryGroups = asks (M.elems . dbStorage)

makeAcidic ''Database ['insertGroup, 'queryGroups]

openDatabase :: IO (AcidState Database)
openDatabase = openLocalStateFrom "data" (Database M.empty 0)

closeDatabase :: AcidState Database -> IO ()
closeDatabase = closeAcidState

intervalThread :: Integer -> IO a -> IO ThreadId
intervalThread s a = forkIO loop
	where loop = handle (\(SomeException _) -> loop) (delay (s * 1000000) >> a >> loop)

main :: IO ()
main =
	bracket openDatabase closeDatabase $ \db -> do
		intervalThread 60 $ do
			putStrLn "Backing up ..."
			createCheckpoint db
			putStrLn "Done"
		acidServer skipAuthenticationCheck (UnixSocket "/tmp/hrel-database") db
