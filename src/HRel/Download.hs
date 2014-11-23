module HRel.Download (
	downloadText,
	downloadNode
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Network.HTTP.Conduit

import HRel.Markup

-- | Download something via HTTP and decode using UTF-8.
downloadText :: String -> IO T.Text
downloadText = fmap T.decodeUtf8 . simpleHttp

-- | Download something via HTTP and parse it.
downloadNode :: String -> IO (Node T.Text)
downloadNode = fmap (parseNode . T.decodeUtf8) . simpleHttp
