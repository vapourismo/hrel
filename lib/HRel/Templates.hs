{-# LANGUAGE OverloadedStrings #-}

module HRel.Templates (
	indexTemplate,
	listTemplate,
	formTemplate
) where

import           Control.Monad

import           Data.Monoid
import qualified Data.Text      as T
import           Data.Time

import           Lucid

import           HRel.Data.Feed
import           HRel.Units

sharedBodyTemplate :: Html () -> Html ()
sharedBodyTemplate contents =
	doctypehtml_ $ do
		head_ $
			link_ [href_ "/style.css", rel_  "stylesheet", type_ "text/css"]
		body_ contents

indexTemplate :: [Feed] -> Html ()
indexTemplate feeds =
	sharedBodyTemplate $
		div_ [class_ "content"] $ do
			forM_ feeds $ \ (Feed fid url) ->
				div_ [class_ "entry"] $
					a_ [class_ "box feed", href_ (T.pack ("/feed/" <> show fid))] $
						toHtml (show url)

			div_ [class_ "footer"] $
				a_ [href_ "/submit", class_ "submit"]
					"Track my feed!"

formatTimeString :: UTCTime -> String
formatTimeString =
	formatTime defaultTimeLocale "%c"

listTemplate :: [(T.Text, T.Text, Maybe Word, UTCTime)] -> Html ()
listTemplate links =
	sharedBodyTemplate $
		div_ [class_ "content"] $ do
			forM_ links $ \ (name, link, mbSize, insertTime) -> do
				div_ [class_ "entry"] $ do
					div_ [class_ "box name"] $
						toHtml name
					div_ [class_ "box size"] $
						toHtml (maybe "unknown" showAsBytes mbSize)
					div_ [class_ "box time"] $
						toHtml (formatTimeString insertTime)
					a_ [class_ "box link", href_ link]
						"link"
					let premLink = "https://www.premiumize.me/downloader?magnet=" <> link
					a_ [class_ "box link", href_ premLink, target_ "blank"]
						"add"

			when (null links) $
				div_ [class_ "footer"] $
					span_ [class_ "error"]
						"Nothing listed? Don't worry. The feed might not have been processed yet."

formTemplate :: Bool -> Html ()
formTemplate invalidURL =
	sharedBodyTemplate $
		div_ [class_ "content"] $
			form_ [method_ "post"] $ do
				div_ [class_ "entry"] $ do
					div_ [class_ "box label"]
						"URL"
					div_ [class_ "box input"] $
						input_ [class_ "text", name_ "url", type_ "text"]
				div_ [class_ "footer"] $
					input_ [class_ "submit", type_ "submit", value_ "Track"]

				when invalidURL $
					div_ [class_ "footer"] $
						span_ [class_ "error"]
							"The given URL is either invalid or points to an unusable feed"
