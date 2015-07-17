{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad

import qualified Lucid                         as L

import           Data.Char
import           Data.Word
import           Data.Monoid
import qualified Data.Text                     as T

import           Web.Scotty

import           System.Environment

import           HRel.Units
import           HRel.Database

sharedBodyTemplate :: L.Html () -> L.Html ()
sharedBodyTemplate contents =
	L.doctypehtml_ $ do
		L.head_ $
			L.link_ [L.href_ "/style.css", L.rel_  "stylesheet", L.type_ "text/css"]
		L.body_ contents

indexTemplate :: [(Word64, T.Text)] -> L.Html ()
indexTemplate feeds =
	sharedBodyTemplate $ do
		L.div_ [L.class_ "content"] $
			L.div_ [L.class_ "entry"] $
				forM_ feeds $ \ (fid, url) ->
					L.a_ [L.class_ "box feed", L.href_ (T.pack ("/feed/" <> show fid))] $
						L.toHtml url

handleIndex :: Database -> ActionM ()
handleIndex db = do
	feeds <- runAction db (query_ "SELECT id, url FROM feeds")
	html (L.renderText (indexTemplate feeds))

listTemplate :: [(T.Text, T.Text, Maybe Word)] -> L.Html ()
listTemplate links =
	sharedBodyTemplate $ do
		L.div_ [L.class_ "content"] $
			forM_ links $ \ (name, link, mbSize) -> do
				let premLink = "https://www.premiumize.me/downloader?magnet=" <> link

				L.div_ [L.class_ "entry"] $ do
					L.div_ [L.class_ "box name"] (L.toHtml name)
					L.div_ [L.class_ "box size"] (L.toHtml (maybe "unknown" showAsBytes mbSize))
					L.a_ [L.class_ "box link", L.href_ link] "link"
					L.a_ [L.class_ "box link", L.href_ premLink, L.target_ "blank"] "add"

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

formTemplate :: L.Html ()
formTemplate =
	L.body_ [L.href_ "Herro"] ""

handleForm :: Database -> ActionM ()
handleForm _ =
	html (L.renderText formTemplate)

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
			get "/submit" (handleForm db)
			--post "/submit" (handleForm db)

			-- Specify list
			get "/feed/:fid" (handleList db)
