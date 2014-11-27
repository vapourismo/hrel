{-# LANGUAGE OverloadedStrings #-}

module HRel.Database (
	connectToDatabase,
	insertGroup,

	module Database.MySQL.Simple
) where

import Data.Word
import Data.Char
import qualified Data.Text.Lazy as T

import Control.Monad

import Network.URI (URI)

import Database.MySQL.Simple

-- |
connectToDatabase :: IO Connection
connectToDatabase =
	connect (defaultConnectInfo {connectHost     = "localhost",
	                             connectDatabase = "hrel"})

-- |
createGroup :: Connection -> IO Word64
createGroup db = do
	execute_ db "INSERT INTO groups () VALUES ()"
	insertID db

-- |
insertGroup :: Connection -> [T.Text] -> [URI] -> IO Word64
insertGroup db names links = do
	rs <- query db "SELECT groupID FROM names WHERE searchName IN ?"
	               (Only (In searchNames))
	groupID <- createGroup db

	forM_ rs $ \(Only oldGroupID) -> do
		execute db "UPDATE names SET groupID = ? WHERE groupID = ?"
		        (groupID, oldGroupID :: Word64)
		execute db "UPDATE links SET groupID = ? WHERE groupID = ?"
		        (groupID, oldGroupID :: Word64)
		execute db "DELETE FROM groups WHERE id = ?"
		        (Only oldGroupID)

	executeMany db "INSERT IGNORE INTO names (searchName, fullName, groupID) VALUES (?, ?, ?)"
	            (zipWith (\s f -> (s, f, groupID)) searchNames names)
	executeMany db "INSERT IGNORE INTO links (uri, groupID) VALUES (?, ?)"
	            (map (flip (,) groupID . show) links)

	return groupID
	where
		searchNames = map (T.map toLower) names
