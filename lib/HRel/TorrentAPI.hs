{-# LANGUAGE OverloadedStrings, TypeApplications, BangPatterns #-}

module HRel.TorrentAPI where

import           Control.Monad.Except

import           Data.Aeson
import           Data.IORef

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.HTTP.Types.QueryLike

import           HRel.Network
import           HRel.Torrents

-- |
data TorrentApiError
	= BadResponse Request String
	| HttpError HttpError
	deriving (Show)

-- |
type TorrentApiM r = ExceptT TorrentApiError IO r

-- | Request manager for the TorrentAPI
data TorrentApi = TorrentApi {
	taManager      :: Manager,
	taTokenStorage :: IORef (Maybe Token)
}

-- | Create a new manager for the TorrentAPI.
newTAManager :: Manager -> IO TorrentApi
newTAManager mgr =
	TorrentApi mgr <$> newIORef Nothing

-- |
performRequest :: (FromJSON a) => Manager -> Request -> TorrentApiM a
performRequest mgr req = do
	contents <- withExceptT HttpError (httpDownload mgr req)
	case eitherDecode (BL.fromChunks contents) of
		Left msg -> throwError (BadResponse req msg)
		Right x  -> pure x

-- |
newtype Token = Token { unToken :: T.Text }
	deriving (Show, Eq, Ord)

instance FromJSON Token where
	parseJSON (Object obj) = Token <$> (obj .: "token")
	parseJSON _ = mempty

-- |
acquireToken :: TorrentApi -> TorrentApiM Token
acquireToken (TorrentApi mgr tokenRef) = do
	mbToken <- lift (readIORef tokenRef)
	case mbToken of
		Just token ->
			pure token

		Nothing -> do
			token <- performRequest mgr tokenRequest
			token <$ lift (writeIORef tokenRef (Just token))
	where
		tokenRequest = parseRequest_ "https://torrentapi.org/pubapi_v2.php?get_token=get_token"

-- |
makeQueryRequest :: (QueryLike q) => Token -> q -> Request
makeQueryRequest (Token tok) params = do
	baseRequest {queryString = renderQuery True (tokQuery : toQuery params)}
	where
		tokQuery = ("token", toQueryValue tok)
		baseRequest = parseRequest_ "https://torrentapi.org/pubapi_v2.php"

-- |
query :: (QueryLike q, FromJSON a) => TorrentApi -> q -> TorrentApiM a
query taMgr params = do
	tok <- acquireToken taMgr
	performRequest (taManager taMgr) (makeQueryRequest tok params)

-- |
newtype SearchTorrent = SearchTorrent { unSearchTorrent :: Torrent }

instance FromJSON SearchTorrent where
	parseJSON (Object obj) = do
		title <- obj .: "filename"
		uri <- obj .: "download"
		pure (SearchTorrent (Torrent title uri))

	parseJSON _ = mempty

-- |
newtype SearchResult = SearchResult { unSearchResult :: [Torrent] }

instance FromJSON SearchResult where
	parseJSON (Object obj) = SearchResult . map unSearchTorrent <$> (obj .: "torrent_results")
	parseJSON _            = mempty

-- |
search :: TorrentApi -> T.Text -> TorrentApiM [Torrent]
search taMgr searchString =
	unSearchResult <$> query @[(T.Text, T.Text)]
	                         taMgr
	                         [("mode",          "search"),
	                          ("search_string", searchString),
	                          ("limit",         "100"),
	                          ("ranked",        "0")]
