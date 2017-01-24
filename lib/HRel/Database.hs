{-# LANGUAGE OverloadedStrings #-}

module HRel.Database (
	Database,
	connectDatabase
) where

import qualified Data.ByteString as B

import qualified Database.PostgreSQL.LibPQ as P

-- | Database connection
type Database = P.Connection

-- | Establish a connection to the database.
connectDatabase :: B.ByteString -> IO Database
connectDatabase = P.connectdb
