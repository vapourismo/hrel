{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad

import           Data.Maybe
import           Data.List
import qualified Data.ByteString as B

import           Network.Http.Client

import           HRel.Sources
import           HRel.Database

downloadReleaseFeed :: B.ByteString -> IO (Maybe [ReleaseName])
downloadReleaseFeed uri =
	get uri $ \ res input ->
		if getStatusCode res == 200 then
			parseReleaseFeed <$> concatHandler res input
		else
			pure Nothing

fetchAllReleases :: Database -> IO ()
fetchAllReleases con = do
	feeds <- listFeeds con
	forM_ feeds $ \ feed -> do
		mbRels <- downloadReleaseFeed (feedURI feed)
		case mbRels of
			Nothing   -> pure ()
			Just rels -> attach (feedID feed) rels
	where
		attach fid rels =
			undefined -- executeMany con "INSERT INTO "

main :: IO ()
main = do
	con <- connectDatabase

	--mbRels <- mapM (downloadReleaseFeed . feedURI) =<< listFeeds con
	--let rels = nub (concat (catMaybes mbRels))

	-- TODO: Insert rels into to-be-processed list/table
	-- TODO: Start

	closeDatabase con
