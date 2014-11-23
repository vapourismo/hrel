{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy as T

import Text.StringLike

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Network.URI

import HRel.Markup
import HRel.Download

toURI :: (Monad m, StringLike s) => s -> MaybeT m URI
toURI = MaybeT . return . parseURI . toString

-- toHost :: (Monad m) => URI -> MaybeT m String
-- toHost = MaybeT . return . fmap uriRegName . uriAuthority

-- trimBS :: B8.ByteString -> B8.ByteString
-- trimBS = B8.filter (not . isSpace)

-- validHoster :: [String]
-- validHoster = ["ul.to", "uploaded.to", "uploaded.net"]

-- ddlvPost :: String -> IO [B8.ByteString]
-- ddlvPost =
-- 	fmap (maybe [] id . runNodeFilter htmlFilter . parseNode) . simpleHttp
-- 	where
-- 		hasAttr k v = attr k >>= guard . (v ==)
-- 		htmlFilter =
-- 			relativeTag "div" $ do
-- 				hasAttr "class" "cont cl"
-- 				relativeTags "a" $ do
-- 					hasAttr "class" "ext-link"

-- 					href <- fmap trimBS $ attr "href"
-- 					inner <- fmap trimBS $ text
-- 					guard (inner == href)

-- 					hoster <- toURI href >>= toHost
-- 					guard (elem hoster validHoster)

-- 					return href

trimText :: T.Text -> T.Text
trimText = T.dropAround isSpace

ddlvFeed :: String -> IO [([T.Text], URI, [URI])]
ddlvFeed =
	fmap (maybe [] id . runNodeFilter fitler) . downloadNode
	where
		fitler =
			relativeTag "rss" $ forTag "channel" $ foreachTag "item" $
				(,,) <$> fmap mkNames (forTag "title" text)
				     <*> (forTag "link" text >>= toURI . trimText)
				     <*> foreachTag "enclosure" (attr "url" >>= toURI)


		mkNames = map trimText . T.split (== '&')

main :: IO ()
main =
	ddlvFeed "http://www.ddlvalley.rocks/category/tv-shows/hd-720/feed/"
	>>= print
