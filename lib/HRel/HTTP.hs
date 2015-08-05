module HRel.HTTP (
	Manager,
	withManager,
	withTLSManager,

	Request,
	download,
	download'
) where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status

-- | Do something with a "Manager".
withTLSManager :: (Manager -> IO a) -> IO a
withTLSManager =
	withManager tlsManagerSettings

-- | Download something.
download :: Manager -> String -> IO (Maybe B.ByteString)
download mgr url =
	maybe (pure Nothing) (download' mgr) (parseUrl url)

-- | Download something.
download' :: Manager -> Request -> IO (Maybe B.ByteString)
download' mgr req = do
	res <- httpLbs req mgr
	pure $ case responseStatus res of
		Status 200 _ -> Just (BL.toStrict (responseBody res))
		_            -> Nothing
