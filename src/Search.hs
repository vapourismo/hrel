{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Word
import Data.String
import Data.Char

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Internal as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as H

import Control.Monad
import Control.Monad.IO.Class

import Network.HTTP.Types.URI
import Web.Scotty

import HRel.Database

instance Parsable Word64 where
	parseParam xs =
		if T.all isDigit xs then
			Right (read (T.unpack xs))
		else
			Left "Not a Word64"

searchHtml :: H.Html
searchHtml =
	H.docTypeHtml $ do
		H.head $ do
			H.title "HRel"
			H.link H.! A.rel "stylesheet"
			       H.! A.type_ "text/css"
			       H.! A.href "/style.css"
		H.body $
			H.form H.! A.id "container"
			       H.! A.method "post"
			       H.! A.action "/" $ do
				H.input H.! A.type_ "input"
				        H.! A.name "q"
				        H.! A.id "query"

resultHtml :: [(Word64, Word64, T.Text)] -> H.Html
resultHtml results =
	H.docTypeHtml $ do
		H.head $ do
			H.title "HRel"
			H.link H.! A.rel "stylesheet"
			       H.! A.type_ "text/css"
			       H.! A.href "/style.css"
		H.body $
			H.div H.! A.id "container" $
				forM_ results $ \(_, groupID, name) ->
					H.a H.! A.href (fromString ("/" ++ show groupID))
					    H.! A.class_ "group-link" $ H.lazyText name

groupResult :: ([T.Text], [String]) -> H.Html
groupResult (names, links) =
	H.docTypeHtml $ do
		H.head $ do
			H.title "HRel"
			H.link H.! A.rel "stylesheet"
			       H.! A.type_ "text/css"
			       H.! A.href "/style.css"
		H.body $
			H.div H.! A.id "container" $ do
				forM_ names $ \name ->
					H.span H.! A.class_ "release-name" $
						H.lazyText name
				forM_ links $ \link ->
					H.a H.! A.class_ "release-link"
					    H.! A.href (fromString link) $ fromString link

main :: IO ()
main = do
	db <- connectToDatabase
	style <- B.readFile "ext/style.css"

	scotty 3300 $ do
		get "/" $
			html (H.renderHtml searchHtml)

		post "/" $ do
			q <- body
			case lookup "q" (parseSimpleQuery (B.toStrict q)) of
				Just searchTerm -> do
					names <- liftIO (findNames db (splitIntoTags searchTerm))
					html (H.renderHtml (resultHtml names))
				Nothing ->
					redirect "/"

		get "/:gid" $ do
			gid <- param "gid"
			entries <- liftIO (findGroup db gid)
			html (H.renderHtml (groupResult entries))

		get "/style.css" $ do
			setHeader "Content-Type" "text/css"
			raw style

		notFound $
			text "There used to be a dog here"

	where
		splitIntoTags = map T.decodeUtf8 . filter (not . B.null) . B.split 32 . B.fromStrict
