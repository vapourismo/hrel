{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad.Trans

import           Data.Word
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Lazy      as TL

import qualified Lucid               as L

import           Web.Scotty

import           Network.URI         hiding (query)

import           HRel.Processing
import           HRel.Database
import           HRel.Templates
import           HRel.Data.Feed
import           HRel.Source.AtomFeed

handleIndex :: Manifest -> ActionM ()
handleIndex Manifest {..} = do
	feeds <- runAction mDatabase findAllFeeds
	html (L.renderText (indexTemplate feeds))

listQuery :: Query
listQuery =
	"SELECT t.name, t.uri, t.size \
	 \ FROM releases r, feed_contents c, release_links l, torrents t \
	 \ WHERE c.feed = ? AND c.rel = r.id AND l.rel = r.id AND t.id = l.tor \
	 \ GROUP BY t.id \
	 \ ORDER BY r.updateTime DESC \
	 \ LIMIT 200"

handleList :: Manifest -> ActionM ()
handleList Manifest {..} = do
	fid <- param "fid"
	items <- runAction mDatabase (query listQuery (Only (fid :: Word64)))
	html (L.renderText (listTemplate items))

handleForm :: Bool -> ActionM ()
handleForm =
	html . L.renderText . formTemplate

tryFeed :: Manifest -> String -> IO (Maybe Feed)
tryFeed Manifest {..} url = do
	mbRels <- fetchAtomFeed mManager url
	case mbRels of
		Just rels | length rels > 0 ->
			runAction mDatabase (createFeed (fromJust (parseURI url)))

		_ -> pure Nothing

handleSubmit :: Manifest -> ActionM ()
handleSubmit mf@(Manifest {..}) = do
	url <- param "url"
	let validURL = length url <= 255 && maybe False (const True) (parseURI url)

	-- Validate URL
	if validURL then do
		mbFeed <- liftIO (tryFeed mf url)

		-- If insertion was successful, queue feed and redirect to the new feed page
		case mbFeed of
			Just feed -> do
				liftIO (queueProcessFeed mf feed)
				redirect (TL.pack ("/feed/" <> show (feedID feed)))

			Nothing ->
				handleForm True
	else
		handleForm True

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

		-- Form
		get "/submit" (handleForm False)
		post "/submit" (handleSubmit mf)

		--
		notFound (redirect "/")
