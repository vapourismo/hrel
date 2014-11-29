{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Text.Blaze.Html.Renderer.Text

import Control.Monad.IO.Class

import Network.HTTP.Types.URI
import Web.Scotty

import HRel.Database
import HRel.Web.Templates

instance Parsable Word64 where
	parseParam = readEither

main :: IO ()
main = do
	db <- connectToDatabase
	style <- B.readFile "ext/style.css"

	scotty 3300 $ do
		get "/" $
			html (renderHtml searchTpl)

		post "/" $ do
			q <- body
			case lookup "q" (parseSimpleQuery (B.toStrict q)) of
				Just searchTerm -> do
					names <- liftIO (findNames db (splitIntoTags searchTerm))
					html (renderHtml (resultTpl names))
				Nothing ->
					redirect "/"

		get "/g/:gid" $
			param "gid" >>= showGroup db

		get "/n/:name" $
			param "name"
			>>= liftIO . findGroupByMember db
			>>= maybe (redirect "/") (showGroup db)

 		get "/style.css" $ do
			setHeader "Content-Type" "text/css"
			raw style

		notFound $
			redirect "/"

	where
		splitIntoTags =
			filter (not . T.null)
			. T.split (== ' ')
			. T.toLower
			. T.decodeUtf8
			. B.fromStrict

		showGroup db gid = do
			entries <- liftIO (findGroup db gid)
			html (renderHtml (groupTpl entries))

