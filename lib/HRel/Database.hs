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
	groupID <- createGroup db

	rs <- query db "SELECT groupID FROM names WHERE searchName IN ?"
	               (Only (In searchNames))
	forM_ rs $ \(Only oldGroupID) -> do
		execute db "UPDATE names SET groupID = ? WHERE groupID = ?"
		        (groupID, oldGroupID :: Word64)
		execute db "UPDATE links SET groupID = ? WHERE groupID = ?"
		        (groupID, oldGroupID :: Word64)
		execute db "DELETE FROM groups WHERE id = ?"
		        (Only oldGroupID)

	forM_ (zip searchNames names) $ \(s, n) -> do
		r <- execute db "INSERT IGNORE INTO names (searchName, fullName, groupID) VALUES (?, ?, ?)"
		             (s, n, groupID)
		when (r > 0) $ do
			nameID <- insertID db
			case T.split (== '-') s of
				[] ->
					return ()
				xs -> do
					executeMany db "INSERT INTO tags (value, nameID) VALUES (?, ?)"
					            (map (flip (,) nameID . cleanText)
					                  . filter (not . T.null)
					                  . T.split (== '.')
					                  $ T.concat (init xs))
					return ()


	executeMany db "INSERT IGNORE INTO links (uri, groupID) VALUES (?, ?)"
	            (map (flip (,) groupID . show) links)

	return groupID
	where
		searchNames = map (T.map toLower) names
		cleanText = T.filter (not . isSpace)
