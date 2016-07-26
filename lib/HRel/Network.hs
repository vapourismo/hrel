module HRel.Network (
	download
) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8

import qualified Network.HTTP.Types.Status as H
import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Client       as H

-- | Consume the body of a 'Response'. Rejects files that are bigger than 10MiB.
consumeBody :: H.BodyReader -> MaybeT IO B.ByteString
consumeBody br =
	consume 10485760 B.empty
	where
		consume bytesLeft collected = do
			dat <- lift br
			if B.length dat > bytesLeft then
				mzero
			else if B.null dat then
				pure collected
			else
				consume (bytesLeft - B.length dat) (B.append collected dat)

-- | Utility to make downloading easier
type Downloader = ReaderT H.Manager (MaybeT IO)

-- | Run the actions captured inside 'Downloader'.
runDownloader :: H.Manager -> Downloader a -> IO (Maybe a)
runDownloader mgr action =
	runMaybeT (runReaderT action mgr)

-- | Do something with the 'Response' to a 'Request'.
withResponse :: H.Request -> (H.Response H.BodyReader -> Downloader a) -> Downloader a
withResponse req fun =
	ReaderT $ \ mgr -> MaybeT $
		H.withResponse req mgr (\ res -> runDownloader mgr (fun res))

-- | Parse the URL into 'Request'.
makeRequest :: String -> Downloader H.Request
makeRequest url =
	lift (MaybeT (pure (H.parseRequest url)))

-- | Get a specific header value from the 'Response'.
getHeader :: H.Response a -> H.HeaderName -> Downloader B.ByteString
getHeader res name =
	lift (MaybeT (pure (lookup name (H.responseHeaders res))))

-- |
request :: String -> (H.Response H.BodyReader -> Downloader a) -> Downloader a
request url fun = do
	req <- lift (MaybeT (pure (H.parseRequest url)))
	withResponse req (process (5 :: Int))
	where
		process redirectsLeft res =
			case H.responseStatus res of
				H.Status 200 _ ->
					fun res

				H.Status 301 _ | redirectsLeft > 0 -> do
					loc <- getHeader res H.hLocation
					req' <- makeRequest (C8.unpack loc)
					withResponse req' (process (redirectsLeft - 1))

				_ -> mzero

-- | Perform a @GET@ request and return the response body. Rejects responses bigger than 10 MiB and
-- follows only 5 redirections.
download :: H.Manager -> String -> IO (Maybe B.ByteString)
download mgr url =
	runDownloader mgr $ request url $ \ res ->
		lift (consumeBody (H.responseBody res))
