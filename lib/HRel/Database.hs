{-# LANGUAGE OverloadedStrings #-}

module HRel.Database (
	connectToDatabase,
	insertGroup,
	findNames,
	findGroup,

	module Database.MySQL.Simple
) where

import Data.Word
import Data.List
import Data.Char
import qualified Data.Text.Lazy as T

import Control.Monad

import Network.URI (URI)

import Database.MySQL.Simple
import Database.MySQL.Base.Types

-- | Connect to the database.
connectToDatabase :: IO Connection
connectToDatabase =
	connect (defaultConnectInfo {connectHost     = "localhost",
	                             connectDatabase = "hrel",
	                             connectOptions  = [Reconnect True]})

-- | Create a new group and return it's ID.
createGroup :: Connection -> IO Word64
createGroup db = do
	execute_ db "INSERT INTO groups () VALUES ()"
	insertID db

-- | Insert a group of release names associated with download links.
insertGroup :: Connection -> [T.Text] -> [URI] -> IO Word64
insertGroup db names links = do
	groupID <- createGroup db

	-- Find groups that might be already in the database with the same release names
	rs <- query db "SELECT groupID FROM names WHERE searchName IN ? GROUP BY groupID"
	               (Only (In searchNames))

	-- Connect existing release names and associated links with the new group
	forM_ rs $ \(Only oldGroupID) -> do
		-- Move existing release names into the new group
		execute db "UPDATE names SET groupID = ? WHERE groupID = ?"
		        (groupID, oldGroupID :: Word64)

		-- Move associated links into the new group
		execute db "UPDATE links SET groupID = ? WHERE groupID = ?"
		        (groupID, oldGroupID :: Word64)

		-- Delete the old group
		execute db "DELETE FROM groups WHERE id = ?"
		        (Only oldGroupID)

	-- Insert the new release names
	forM_ (zip searchNames names) $ \(s, n) -> do
		r <- execute db "INSERT IGNORE INTO names (searchName, fullName, groupID) VALUES (?, ?, ?)"
		             (s, n, groupID)

		-- Generate search tags, when the insertion was successful (no duplicate entry)
		when (r > 0) $ do
			nameID <- insertID db
			case T.split (== '-') s of
				[] ->
					-- No use of '-' means it's a ill-formated release name
					return ()
				xs -> do
					executeMany db "INSERT INTO tags (value, nameID) VALUES (?, ?)"
					            (tagify nameID xs)
					return ()

	-- Insert links
	executeMany db "INSERT IGNORE INTO links (uri, groupID) VALUES (?, ?)"
	            (map (flip (,) groupID . show) links)

	return groupID
	where
		searchNames = map (T.map toLower) names
		cleanText = T.filter (not . isSpace)
		tagify nameID =
			map (flip (,) nameID . cleanText)   -- Associate with nameID
			. filter (not . T.null . cleanText) -- Filter empty segments
			. T.split (== '.')                  -- Split into tags
			. T.concat
			. init                              -- Remove the name of the releasing group

-- | Search for names that match the given tags.
findNames :: Connection -> [T.Text] -> IO [(Word64, Word64, T.Text)]
findNames db tags =
	query db sql (param, param)
	where
		param = In (map T.toLower tags)
		sql = "SELECT names.nameID, names.groupID, names.fullName FROM tags, names WHERE (tags.value IN ? OR names.searchName IN ?) AND tags.nameID = names.nameID GROUP BY tags.nameID ORDER BY COUNT(tags.value) DESC, names.groupID DESC"

-- | Find a group by ID
findGroup :: Connection -> Word64 -> IO ([T.Text], [String])
findGroup db groupID = do
	names <- query db "SELECT fullName FROM names WHERE groupID = ?" (Only groupID)
	links <- query db "SELECT uri FROM links WHERE groupID = ?" (Only groupID)
	return (map fromOnly names, sort (map fromOnly links))
