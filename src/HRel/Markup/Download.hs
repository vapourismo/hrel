module HRel.Markup.Download (
	module HRel.Markup.Node,

	-- * Download shortcuts
	withNodeFilter,
	withNodeFilter',

	-- * Hungry "NodeFilter"s
	NodeFilterH,
	withNodeFilterH,
	continueWith,
	continueWith',
) where

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString as B

import Network.HTTP.Conduit
import Network.HTTP.Types.Status

import HRel.Markup.Node

-- | Download something and run the node filter (assumes UTF-8).
withNodeFilter :: (MonadIO m)
               => NodeFilterT T.Text m a -> Request -> Manager -> m (Maybe a)
withNodeFilter nfil req mgr = do
	res <- liftIO (httpLbs req mgr)
	case responseStatus res of
		Status 200 _ -> runNodeFilterT nfil (parseNode (T.decodeUtf8 (responseBody res)))
		_ -> return Nothing

-- | Same as "withNodeFilter" but for the "NodeFilter"s.
withNodeFilter' :: NodeFilter T.Text a -> Request -> Manager -> IO (Maybe a)
withNodeFilter' nfil req mgr = do
	res <- httpLbs req mgr
	return $ case responseStatus res of
		Status 200 _ -> runNodeFilter nfil (parseNode (T.decodeUtf8 (responseBody res)))
		_ -> Nothing

-- | A special "NodeFilterT" variant which is optimized for node filters that
--   need to aquire further data during the filtering process.
type NodeFilterH = NodeFilterT T.Text (ReaderT Manager IO)

-- | Apply the node filter and preserve the manager.
runNodeFilterH :: NodeFilterH a -> Node T.Text -> Manager -> IO (Maybe a)
runNodeFilterH nfil node = runReaderT (runNodeFilterT nfil node)

-- | Analog to previous "withNodeFilter*" functions.
withNodeFilterH :: NodeFilterH a -> Request -> Manager -> IO (Maybe a)
withNodeFilterH nfil req mgr = do
	res <- httpLbs req mgr
	case responseStatus res of
		Status 200 _ -> runNodeFilterH nfil (parseNode (T.decodeUtf8 (responseBody res))) mgr
		_ -> return Nothing

-- | "withNodeFilterH" within a "NodeFilterH"
continueWith :: Request -> NodeFilterH a -> NodeFilterH a
continueWith req nfil =
	lift (lift ask) >>= MaybeT . liftIO . withNodeFilterH nfil req

-- | Easier version of "continueWith".
continueWith' :: String -> [(B.ByteString, Maybe B.ByteString)] -> NodeFilterH a -> NodeFilterH a
continueWith' url qry nfil = do
	req <- liftIO (parseUrl url)
	continueWith (setQueryString qry req) nfil
