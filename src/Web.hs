{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.Trans

import qualified Lucid                   as L

import           Data.Char
import           Data.Word
import           Data.Monoid
import           Data.Conduit
import qualified Data.Conduit.List       as C
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL

import           Web.Scotty

import           Network.URI             hiding (query)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           System.Environment

import           HRel.Source.Feeds
import           HRel.Processing
import           HRel.Database
import           HRel.Units

sharedBodyTemplate :: L.Html () -> L.Html ()
sharedBodyTemplate contents =
	L.doctypehtml_ $ do
		L.head_ $
			L.link_ [L.href_ "/style.css", L.rel_  "stylesheet", L.type_ "text/css"]
		L.body_ contents

indexTemplate :: [(Word64, T.Text)] -> L.Html ()
indexTemplate feeds =
	sharedBodyTemplate $
		L.div_ [L.class_ "content"] $ do
			forM_ feeds $ \ (fid, url) ->
				L.div_ [L.class_ "entry"] $
					L.a_ [L.class_ "box feed", L.href_ (T.pack ("/feed/" <> show fid))] $
						L.toHtml url

			L.div_ [L.class_ "footer"] $
				L.a_ [L.href_ "/submit", L.class_ "submit"]
					"Track my RSS feed!"

handleIndex :: Database -> ActionM ()
handleIndex db = do
	feeds <- runAction db (query_ "SELECT id, url FROM feeds")
	html (L.renderText (indexTemplate feeds))

listTemplate :: [(T.Text, T.Text, Maybe Word)] -> L.Html ()
listTemplate links =
	sharedBodyTemplate $
		L.div_ [L.class_ "content"] $ do
			forM_ links $ \ (name, link, mbSize) -> do
				let premLink = "https://www.premiumize.me/downloader?magnet=" <> link
				L.div_ [L.class_ "entry"] $ do
					L.div_ [L.class_ "box name"] $
						L.toHtml name
					L.div_ [L.class_ "box size"] $
						L.toHtml (maybe "unknown" showAsBytes mbSize)
					L.a_ [L.class_ "box link", L.href_ link]
						"link"
					L.a_ [L.class_ "box link", L.href_ premLink, L.target_ "blank"]
						"add"

			when (null links) $
				L.div_ [L.class_ "footer"] $
					L.span_ [L.class_ "error"]
						"Nothing listed yet? Don't worry. The feed might not have been processed."

listQuery :: Query
listQuery =
	"SELECT name, url, size \
	 \ FROM torrents t, releases r \
	 \ WHERE r.feed = ? AND t.rel = r.id \
	 \ ORDER BY t.id DESC \
	 \ LIMIT 100"

handleList :: Database -> ActionM ()
handleList db = do
	fid <- param "fid"
	items <- runAction db (query listQuery (Only (fid :: Word64)))
	html (L.renderText (listTemplate items))

formTemplate :: Bool -> L.Html ()
formTemplate invalidURL =
	sharedBodyTemplate $
		L.div_ [L.class_ "content"] $
			L.form_ [L.method_ "post"] $ do
				L.div_ [L.class_ "entry"] $ do
					L.div_ [L.class_ "box label"]
						"URL"
					L.div_ [L.class_ "box input"] $
						L.input_ [L.class_ "text", L.name_ "url", L.type_ "text"]
				L.div_ [L.class_ "footer"] $
					L.input_ [L.class_ "submit", L.type_ "submit", L.value_ "Track"]

				when invalidURL $
					L.div_ [L.class_ "footer"] $
						L.span_ [L.class_ "error"]
							"The given URL is either invalid or points to an unusable RSS feed"

handleForm :: Bool -> ActionM ()
handleForm =
	html . L.renderText . formTemplate

tryFeed :: Database -> String -> IO (Maybe Word64)
tryFeed db url =
	withManager tlsManagerSettings $ \ mgr -> do
		releases <- fromRSSTitles mgr url $$ C.consume

		if length releases > 0 then do
			-- Add to existing feeds
			mbID <- runAction db (insert "INSERT INTO feeds (url) VALUES (?) \
			                              \ ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID(id)"
			                             (Only url))

			case mbID of
				Just fid -> do
					-- Fetch torrents
					runConduit $
						C.sourceList (map ((,) fid) releases)
							=$= trackReleases db
							=$= findTorrents mgr
							=$= trackTorrents db
					pure (Just fid)

				x -> pure x
		else
			pure Nothing

handleSubmit :: Database -> ActionM ()
handleSubmit db = do
	url <- param "url"
	let validURL = length url <= 255 && maybe False (const True) (parseURI url)

	-- Validate URL
	if validURL then do
		mbID <- liftIO (tryFeed db url)

		-- If insertion was successful, redirect to the new feed page
		case mbID of
			Just fid -> redirect (TL.pack ("/feed/" <> show fid))
			Nothing  -> handleForm True
	else
		handleForm True

main :: IO ()
main = do
	args <- getArgs

	-- Figure out which port to use
	let scottyPort =
		case args of
			[portStr] | all isDigit portStr -> read portStr
			_ -> 3000

	withDatabase $ \ db -> do
		-- Launch Scotty
		scotty scottyPort $ do
			-- Static
			get "/style.css" $ do
				setHeader "Content-Type" "text/css"
				file "assets/style.css"

			-- Index
			get "/" (handleIndex db)

			-- Form
			get "/submit" (handleForm False)
			post "/submit" (handleSubmit db)

			-- Specify list
			get "/feed/:fid" (handleList db)

			--
			notFound (redirect "/")
