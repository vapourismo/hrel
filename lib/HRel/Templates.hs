{-# LANGUAGE OverloadedStrings #-}

module HRel.Templates (
	indexTemplate,
	listTemplate
) where

import           Control.Monad

import           Data.Monoid
import qualified Data.Text     as T
import           Data.Word

import           HRel.Units
import           Lucid

sharedBodyTemplate :: Html () -> Html ()
sharedBodyTemplate contents =
	doctypehtml_ $ do
		head_ $
			link_ [href_ "/style.css", rel_  "stylesheet", type_ "text/css"]
		body_ contents

indexTemplate :: [(Word64, T.Text)] -> Html ()
indexTemplate feeds =
	sharedBodyTemplate $
		div_ [class_ "content"] $ do
			forM_ feeds $ \ (fid, url) ->
				div_ [class_ "entry"] $
					a_ [class_ "box feed", href_ (T.pack ("/feed/" <> show fid))] $
						toHtml url

			div_ [class_ "footer"] $
				a_ [href_ "/submit", class_ "submit"]
					"Track my feed!"

listTemplate :: [(T.Text, T.Text, Maybe Word)] -> Html ()
listTemplate links =
	sharedBodyTemplate $
		div_ [class_ "content"] $ do
			forM_ links $ \ (name, link, mbSize) -> do
				let premLink = "https://www.premiumize.me/downloader?magnet=" <> link
				div_ [class_ "entry"] $ do
					div_ [class_ "box name"] $
						toHtml name
					div_ [class_ "box size"] $
						toHtml (maybe "unknown" showAsBytes mbSize)
					a_ [class_ "box link", href_ link]
						"link"
					a_ [class_ "box link", href_ premLink, target_ "blank"]
						"add"

			when (null links) $
				div_ [class_ "footer"] $
					span_ [class_ "error"]
						"Nothing listed? Don't worry. The feed might not have been processed yet."
