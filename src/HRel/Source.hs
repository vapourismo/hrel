{-# LANGUAGE OverloadedStrings #-}

module HRel.Source (
	-- * Release
	Release,
	toText,
	makeRelease,
	normalizeRelease,

	-- * Utilities
	withTextResponse,
	module Network.HTTP.Client,
	module Network.HTTP.Types,

	-- * Torrent
	Torrent (..),
	Aggregator (..),
	fetch
) where

import Control.Exception

import Data.Char
import Data.List

import qualified Data.ByteString as B

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.URI

import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Units

-- | Release Identifier
newtype Release = Release { toText :: T.Text }
	deriving (Show, Eq, Ord)

-- | Make a release from the given raw text.
makeRelease :: T.Text -> Release
makeRelease = Release . normalizeRelease

-- | Normalize the release name.
normalizeRelease :: T.Text -> T.Text
normalizeRelease =
	normalizeName . fst . retrieveAuthor
	where
		splitProperly f = filter (not . T.null) . map T.strip . T.split f

		retrieveAuthor txt =
			case splitProperly (== '-') txt of
				[rn] -> (rn, Nothing)
				xs   -> (T.intercalate "-" (init xs),
				         Just (fst (T.span (not . isSpace) (last xs))))

		normalizeName =
			T.toLower . T.intercalate " " . splitProperly (not . isAlphaNum)

-- | Do something with a "Text" response.
withTextResponse :: Request -> Manager -> (T.Text -> IO [a]) -> IO [a]
withTextResponse req mgr f =
	handle (\ (SomeException _) -> return []) $
		withResponse req mgr $ \ res ->
			case responseStatus res of
				Status 200 _ ->
					brConsume (responseBody res) >>= f . T.decodeUtf8 . B.concat

				Status _   _ ->
					return []

-- | Torrent
data Torrent = Torrent {
	torrentRelease     :: Release,
	torrentSource      :: [URI],
	torrentContentSize :: Maybe Word
} deriving (Eq, Ord)

instance Show Torrent where
	show (Torrent rel uris size) =
		"Release '" ++ T.unpack (toText rel) ++ "'" ++
		maybe [] (\ s -> " (" ++ showAsBytes s ++ ")") size ++ "\n" ++
		intercalate "\n" (map (\ u -> " + " ++ show u) uris)

-- | Used to aggregate "a".
newtype Aggregator a = Aggregator { runAggregator :: Manager -> IO [a] }

-- | "Monoid" instance which can be used to merge several "Aggregator"s.
instance Monoid (Aggregator a) where
	mempty = Aggregator (const (pure []))
	mappend (Aggregator a) (Aggregator b) =
		Aggregator (\ mgr -> (++) <$> a mgr <*> b mgr)
	mconcat as = Aggregator (\ mgr -> fmap concat (mapM (\ (Aggregator f) -> f mgr) as))

-- | Fetch "Torrent"s.
fetch :: Aggregator a -> IO [a]
fetch = withManager tlsManagerSettings . runAggregator
