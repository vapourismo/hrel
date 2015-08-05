module HRel.HTTP (
	Manager,
	newManager,
	newTLSManager,

	Request,
	download,
	download'
) where

import           Control.Exception

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status

-- | Do something with a "Manager".
newTLSManager :: IO Manager
newTLSManager =
	newManager tlsManagerSettings

-- | Download something.
download :: Manager -> String -> IO (Maybe B.ByteString)
download mgr url =
	maybe (pure Nothing) (download' mgr) (parseUrl url)

-- | Download something.
download' :: Manager -> Request -> IO (Maybe B.ByteString)
download' mgr req =
	handle (\ (SomeException _) -> pure Nothing) $ do
		res <- httpLbs req mgr
		pure $ case responseStatus res of
			Status 200 _ -> Just (BL.toStrict (responseBody res))
			_            -> Nothing
