{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad

import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as H
import qualified Text.Blaze.Html.Renderer.Text as H

import           Data.Word
import           Data.Bits
import           Data.Monoid
import           Data.Default.Class
import qualified Data.Text                     as T

import           Web.Scotty
import           Network.Socket

import           System.Posix.Files

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
			forM_ links $ \ (name, link, mbSize) -> do
				let premLink = "https://www.premiumize.me/downloader?magnet=" <> link

				H.div H.! H.class_ "entry" $ do
					H.div H.! H.class_ "box name" $ H.text name
					H.div H.! H.class_ "box size" $ H.string (maybe "unknown" showAsBytes mbSize)
					H.a H.! H.class_ "box link"
					    H.! H.href (H.toValue link) $
						H.text "link"
					H.a H.! H.class_ "box link"
					    H.! H.href (H.toValue premLink)
					    H.! H.target "blank" $
						H.text "add"

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

unixSocket :: FilePath
unixSocket = "web.sock"

main :: IO ()
main =
	withDatabase $ \ db -> do
		-- Remove unix socket if it exists
		socketExists <- fileExist unixSocket
		when socketExists (removeLink unixSocket)

		-- Setup socket
		sock <- socket AF_UNIX Stream 0
		bind sock (SockAddrUnix unixSocket)
		listen sock sOMAXCONN

		-- Modify socket mode
		setFileMode unixSocket $
			ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
			groupReadMode .|. groupWriteMode .|. groupExecuteMode

		-- Launch Scotty
		scottySocket def sock $ do
			-- Static
			get "/style.css" $ do
				setHeader "Content-Type" "text/css"
				file "assets/style.css"

			-- Index
			get "/" (handleIndex db)

			-- Specify list
			get "/feed/:fid" (handleList db)
