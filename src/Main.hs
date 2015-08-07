{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad.Trans
import           Control.Applicative

import           Data.Word
import           Data.Monoid
import qualified Data.Text.Lazy      as TL

import qualified Lucid               as L

import           Web.Scotty

import           Network.URI         hiding (query)

import           HRel.Config
import           HRel.Database
import           HRel.Templates
import           HRel.Processing

import           HRel.Data.Feed
import           HRel.Data.Release

import           HRel.Source.AtomFeed

handleIndex :: Manifest -> ActionM ()
handleIndex Manifest {..} = do
	mbFeeds <- runAction mDatabase findAllFeeds
	html (L.renderText (indexTemplate (maybe [] id mbFeeds)))

listQuery :: Query
listQuery =
	"SELECT t.name, t.uri, t.size, t.insertTime \
	 \ FROM releases r, feed_contents c, release_links l, torrents t \
	 \ WHERE c.feed = ? AND c.rel = r.id AND l.rel = r.id AND t.id = l.tor \
	 \ GROUP BY t.id \
	 \ ORDER BY t.insertTime DESC, r.name ASC \
	 \ LIMIT 200"

handleList :: Manifest -> ActionM ()
handleList Manifest {..} = do
	fid <- param "fid"
	mbItems <- runAction mDatabase (query listQuery (Only (fid :: Word64)))
	html (L.renderText (listTemplate (maybe [] id mbItems)))

handleForm :: Bool -> ActionM ()
handleForm =
	html . L.renderText . formTemplate

tryFeed :: Manifest -> String -> IO (Maybe Feed)
tryFeed mf@(Manifest {..}) url = do
	mbRels <- fetchAtomFeed mManager url
	case (,) <$> mbRels <*> parseURI url of
		Just (names, uri) | length names > 0 -> do
			mbFeed <- runAction mDatabase (findFeedByURI uri <|> createNew uri names)
			maybe (pure ()) (queueCommand mf . MatchTorrentsFor) mbFeed
			pure mbFeed

		_ -> pure Nothing
	where
		createNew uri names = do
			feed <- createFeed uri
			rels <- mapM createRelease names
			addReleases feed rels
			pure feed


handleSubmit :: Manifest -> ActionM ()
handleSubmit mf@(Manifest {..}) = do
	url <- param "url"
	let validURL = length url <= 255 && maybe False (const True) (parseURI url)

	-- Validate URL
	if validURL then do
		mbFeed <- liftIO (tryFeed mf url)

		-- If insertion was successful, queue feed and redirect to the new feed page
		case mbFeed of
			Just feed ->
				redirect (TL.pack ("/feed/" <> show (feedID feed)))

			Nothing ->
				handleForm True
	else
		handleForm True

main :: IO ()
main = withManifest $ \ mf -> do
	spawnWorkers mf
	spawnJobTimer mf

	queueCommand mf ProcessAllFeeds
	maybe (pure ()) (queueCommand mf . ProcessHourlyDump) confHourlyDump

	scotty confListenPort $ do
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
