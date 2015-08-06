{-# LANGUAGE ScopedTypeVariables #-}

module HRel.HTTP (
	Manager,
	newManager,
	newTLSManager,

	Request,
	download,
	downloadGZip,
) where

import           Control.Exception

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL

import           Codec.Compression.GZip    as G

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
	fmap BL.toStrict <$> maybe (pure Nothing) (download' mgr) (parseUrl url)

-- | Download something gzipped.
downloadGZip :: Manager -> String -> IO (Maybe B.ByteString)
downloadGZip mgr url =
	fmap (BL.toStrict . G.decompress) <$> maybe (pure Nothing) (download' mgr) (parseUrl url)

-- | Download something.
download' :: Manager -> Request -> IO (Maybe BL.ByteString)
download' mgr req =
	handle noteException $ do
		res <- httpLbs req mgr
		pure $ case responseStatus res of
			Status 200 _ -> Just (responseBody res)
			_            -> Nothing
	where
		noteException (SomeException e) = do
			putStrLn ("download': " ++ show req ++ " threw " ++ show e)
			pure Nothing
