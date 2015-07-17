{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad

import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as H
import qualified Text.Blaze.Html.Renderer.Text as H

import           Data.Word
import           Data.Monoid
import qualified Data.Text                     as T

import           Web.Scotty

import           HRel.Units
import           HRel.Database

sharedBodyTemplate :: H.Html -> H.Html
sharedBodyTemplate contents =
	H.docTypeHtml $ do
		H.head $
			H.link H.! H.href  "/style.css"
			       H.! H.rel   "stylesheet"
			       H.! H.type_ "text/css"

		H.body contents

indexTemplate :: [(Word64, T.Text)] -> H.Html
indexTemplate feeds =
	sharedBodyTemplate $ do
		H.div H.! H.class_ "content" $
			H.div H.! H.class_ "entry" $
				forM_ feeds $ \ (fid, url) ->
					H.a H.! H.class_ "box feed" H.! H.href ("/feed/" <> H.toValue fid) $
						H.text url

handleIndex :: Database -> ActionM ()
handleIndex db = do
	feeds <- runAction db (query_ "SELECT id, url FROM feeds")
	html (H.renderHtml (indexTemplate feeds))

listTemplate :: [(T.Text, T.Text, Maybe Word)] -> H.Html
listTemplate links =
	sharedBodyTemplate $ do
		H.div H.! H.class_ "content" $
			forM_ links $ \ (name, link, mbSize) ->
				H.div H.! H.class_ "entry" $ do
					H.div H.! H.class_ "box name" $ H.text name
					H.div H.! H.class_ "box size" $ H.string (maybe "unknown" showAsBytes mbSize)
					H.a H.! H.class_ "box link" H.! H.href (H.toValue link) $
						H.text "link"

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
	html (H.renderHtml (listTemplate items))

main :: IO ()
main = withDatabase $ \ db -> scotty 8080 $ do
	-- Static
	get "/style.css" $ do
		setHeader "Content-Type" "text/css"
		file "assets/style.css"

	-- Index
	get "/" (handleIndex db)

	-- Specify list
	get "/feed/:fid" (handleList db)
