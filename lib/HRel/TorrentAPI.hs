{-# LANGUAGE OverloadedStrings, ExistentialQuantification, TypeApplications, BangPatterns #-}

module HRel.TorrentAPI where

import           Control.Exception
import           Control.Monad.Except
import           Control.Concurrent

import           Data.Aeson

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import           Network.HTTP.Client
import           Network.HTTP.Types
import           Network.HTTP.Types.QueryLike

import           System.Mem.Weak

import           HRel.Network
import           HRel.Torrents

-- | API request
data TARequest = TARequest !Request !(MVar BL.ByteString)

-- | Request manager for the TorrentAPI
newtype TAManager = TAManager { performRequest :: Request -> IO BL.ByteString }

-- | Create a new manager for the TorrentAPI. Second parameter determines whether the API request
-- limit shall be respected.
newTAManager :: Manager -> Bool -> IO TAManager
newTAManager mgr True = do
	chan <- newChan
	let m = TAManager (performer chan)
	wm <- mkWeakPtr m Nothing
	m <$ forkIO (processor chan wm)
	where
		performer chan req = do
			dest <- newEmptyMVar
			writeChan chan (TARequest req dest)
			takeMVar dest

		processor chan wm = do
			mbm <- deRefWeak wm
			case mbm of
				Just _  -> iterate chan mgr >> processor chan wm
				Nothing -> pure ()

		iterate chan mgr = do
			TARequest req dest <- readChan chan
			handle (\ (SomeException _) -> putMVar dest BL.empty) $ do
				chunks <- httpDownload mgr req
				let result = BL.fromChunks chunks
				result `seq` putMVar dest result

			threadDelay 2000000

newTAManager mgr False =
	pure (TAManager (\ req -> BL.fromChunks <$> httpDownload mgr req))

data TAError
	= BadResponse Request String
	deriving (Show)

type TA m r = ExceptT TAError m r

query :: (MonadIO m, FromJSON a) => TAManager -> Request -> TA m a
query taMgr req = do
	contents <- liftIO (performRequest taMgr req)
	case eitherDecode contents of
		Left msg -> throwError (BadResponse req msg)
		Right x  -> pure x

newtype Token = Token T.Text
	deriving (Show, Eq, Ord)

instance FromJSON Token where
	parseJSON (Object obj) = Token <$> (obj .: "token")
	parseJSON _ = mempty

tokenRequest :: Request
tokenRequest = parseRequest_ "https://torrentapi.org/pubapi_v2.php?get_token=get_token"

fetchToken :: (MonadIO m) => TAManager -> TA m Token
fetchToken taMgr =
	query taMgr tokenRequest

baseRequest :: Request
baseRequest = parseRequest_ "https://torrentapi.org/pubapi_v2.php"

makeRequest :: (QueryLike q) => Token -> q -> Request
makeRequest (Token tok) params = do
	baseRequest {queryString = renderQuery True (tokQuery : toQuery params)}
	where
		tokQuery = ("token", toQueryValue tok)

query_ :: (QueryLike q, MonadIO m, FromJSON a) => TAManager -> Token -> q -> TA m a
query_ taMgr tok params =
	query taMgr (makeRequest tok params)

newtype SearchTorrent = SearchTorrent { unSearchTorrent :: Torrent }

instance FromJSON SearchTorrent where
	parseJSON (Object obj) = do
		title <- obj .: "filename"
		uri <- obj .: "download"
		pure (SearchTorrent (Torrent title uri))

	parseJSON _ = mempty

newtype SearchResult = SearchResult { unSearchResult :: [Torrent] }

instance FromJSON SearchResult where
	parseJSON (Object obj) = SearchResult . map unSearchTorrent <$> (obj .: "torrent_results")
	parseJSON _            = mempty

search :: (MonadIO m) => TAManager -> Token -> T.Text -> TA m [Torrent]
search taMgr token searchString =
	unSearchResult <$> query_ @[(T.Text, T.Text)]
	                          taMgr
	                          token
	                          [("mode",          "search"),
	                           ("search_string", searchString),
	                           ("limit",         "100"),
	                           ("ranked",        "0")]
