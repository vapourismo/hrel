{-# LANGUAGE OverloadedStrings #-}

module HRel.Database.SQL.SelectSpec (spec) where

import qualified Database.PostgreSQL.Simple as Database

import Test.Hspec

setupDatabase :: IO Database.Connection
setupDatabase = do
    conn <- Database.connectPostgreSQL "host=localhost dbname=hrel_test user=hrel"

    _ <-
        Database.execute_
            conn
            "CREATE TABLE IF NOT EXISTS table0 (x INTEGER NOT NULL, y VARCHAR NOT NULL)"

    _ <-
        Database.execute
            conn
            "INSERT INTO table0 (x,y) VALUES (?, ?), (?, ?), (?, ?), (?, ?)"
            ( 13      :: Int
            , "Hello" :: String
            , 37      :: Int
            , "World" :: String
            , 26      :: Int
            , "Ole"   :: String
            , 0       :: Int
            , ""      :: String
            )

    pure conn

cleanUpDatabase :: Database.Connection -> IO ()
cleanUpDatabase conn = do
    _ <- Database.execute_ conn
        "DROP TABLE table0"

    Database.close conn

spec :: Spec
spec = beforeAll setupDatabase $ afterAll cleanUpDatabase $
    describe "Statement" $ do
        pure ()
