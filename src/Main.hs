{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char
import Data.List
import qualified Data.Text.Lazy as T

import Text.StringLike

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Network.URI

import HRel.Markup
import HRel.Download

toURI :: (Monad m, StringLike s) => s -> MaybeT m URI
toURI = MaybeT . return . parseURI . toString

trimText :: T.Text -> T.Text
trimText = T.dropAround isSpace

toHost :: (Monad m) => URI -> MaybeT m String
toHost = MaybeT . return . fmap uriRegName . uriAuthority

validHoster :: [String]
validHoster = ["ul.to", "uploaded.to", "uploaded.net",
               "uptobox.com", "rapidgator.net", "go4up.com"]

ddlValleyPostFilter :: NodeFilter T.Text [URI]
ddlValleyPostFilter =
	relativeTag "div" $ do
		attrDiv <- attr "class"
		guard (attrDiv == "cont cl")
		relativeTags "a" $ do
			attrA <- attr "class"
			guard (attrA == "ext-link")

			href <- fmap trimText $ attr "href"
			inner <- fmap trimText $ text
			guard (inner == href)

			uri <- toURI href
			hoster <- toHost uri
			guard (elem hoster validHoster)

			return uri

ddlValleyRSSFilter :: NodeFilterT T.Text IO [(,) [T.Text] [URI]]
ddlValleyRSSFilter =
	relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
		(,) <$> fmap (map trimText . T.split (== '&')) (forTag "title" text)
		    <*> (mergeLinks <$> (forTag "link" text
		                         >>= liftIO . ddlValleyPost
		                                    . T.unpack . trimText)
		                    <*> foreachTag "enclosure" (attr "url" >>= toURI))
	where
		mergeLinks a b = nub (a ++ b)

ddlValleyPost :: String -> IO [URI]
ddlValleyPost =
	fmap (maybe [] id . runNodeFilter ddlValleyPostFilter) . downloadNode

ddlValleyRSS :: String -> IO [(,) [T.Text] [URI]]
ddlValleyRSS loc = do
	cnts <- downloadNode loc
	res <- runNodeFilterT ddlValleyRSSFilter cnts
	return (maybe [] id res)

main :: IO ()
main =
	ddlValleyRSS "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/"
	>>= print
