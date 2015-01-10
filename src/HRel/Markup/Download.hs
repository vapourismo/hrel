module HRel.Markup.Download (
	withNodeFilter,
	withNodeFilter'
) where

import Control.Monad.IO.Class

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Network.HTTP.Conduit

import HRel.Markup.Node

-- | Download something via HTTP and run the node filter (assumes UTF-8).
withNodeFilter :: (MonadIO m)
               => NodeFilterT T.Text m a -> String -> m (Maybe a)
withNodeFilter f url =
	liftIO (simpleHttp url)
	>>= runNodeFilterT f . parseNode . T.decodeUtf8

-- | Same as "withNodeFilter" but for the "NodeFilter"s.
withNodeFilter' :: NodeFilter T.Text a -> String -> IO (Maybe a)
withNodeFilter' f url =
	fmap (runNodeFilter f . parseNode . T.decodeUtf8) (simpleHttp url)

