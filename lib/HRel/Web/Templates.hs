{-# LANGUAGE OverloadedStrings #-}

module HRel.Web.Templates (
	searchTpl,
	resultTpl,
	groupTpl
) where

import Prelude hiding (span, div, head)

import Data.String
import Data.Word
import qualified Data.Text.Lazy as T

import Text.Blaze.Html5
import Text.Blaze.Internal
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad

-- | Search Page
searchTpl :: Html
searchTpl =
	docTypeHtml $ do
		head $ do
			title "HRel"
			link ! A.rel "stylesheet"
			     ! A.type_ "text/css"
			     ! A.href "/style.css"
		body $
			form ! A.id "container"
			     ! A.method "post"
			     ! A.action "/" $ do
				input ! A.type_ "input"
				      ! A.name "q"
				      ! A.id "query"

-- | Result Page
resultTpl :: [(Word64, Word64, T.Text)] -> Html
resultTpl results =
	docTypeHtml $ do
		head $ do
			title "HRel"
			link ! A.rel "stylesheet"
			     ! A.type_ "text/css"
			     ! A.href "/style.css"
		body $
			div ! A.id "container" $
				forM_ results $ \(_, groupID, name) ->
					a ! A.href (fromString ("/" ++ show groupID))
					  ! A.class_ "group-link" $ lazyText name

-- | Group Inspection Page
groupTpl :: ([T.Text], [String]) -> Html
groupTpl (names, links) =
	docTypeHtml $ do
		head $ do
			title "HRel"
			link ! A.rel "stylesheet"
			     ! A.type_ "text/css"
			     ! A.href "/style.css"
		body $
			div ! A.id "container" $ do
				forM_ names $ \name ->
					span ! A.class_ "release-name" $
						lazyText name
				forM_ links $ \l ->
					a ! A.class_ "release-link"
					  ! A.href (fromString l) $ fromString l
