{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Word

import qualified Lucid               as L

import           Web.Scotty

import           HRel.Processing
import           HRel.Database
import           HRel.Templates
import           HRel.Data.Feed

handleIndex :: Manifest -> ActionM ()
handleIndex Manifest {..} = do
	feeds <- runAction mDatabase findAllFeeds
	html (L.renderText (indexTemplate feeds))

listQuery :: Query
listQuery =
	"SELECT t.name, t.uri, t.size \
	 \ FROM releases r, feed_contents c, release_links l, torrents t \
	 \ WHERE c.feed = ? AND c.rel = r.id AND l.rel = r.id AND t.id = l.tor \
	 \ ORDER BY r.updateTime DESC \
	 \ LIMIT 200"

handleList :: Manifest -> ActionM ()
handleList Manifest {..} = do
	fid <- param "fid"
	items <- runAction mDatabase (query listQuery (Only (fid :: Word64)))
	html (L.renderText (listTemplate items))

main :: IO ()
main = withManifest $ \ mf -> do
	spawnWorkers mf
	spawnJobTimer mf
	--processAllFeeds mf

	scotty 3000 $ do
		get "/style.css" $ do
			setHeader "Content-Type" "text/css"
			file "assets/style.css"

		-- Index
		get "/" (handleIndex mf)

		-- Specify list
		get "/feed/:fid" (handleList mf)
