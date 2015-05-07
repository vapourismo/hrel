{-# LANGUAGE OverloadedStrings #-}

module HRel.Source (
	-- * Release
	Release,
	makeRelease,
	normalizeRelease,

	-- * Torrent
	Torrent (..),
	Aggregator (..),
	fetch
) where

import Data.Char
import qualified Data.Text as T

import Network.URI
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- | Release Identifier
newtype Release = Release T.Text
	deriving (Show, Eq, Ord)

-- | Make a release from the given raw text.
makeRelease :: T.Text -> Release
makeRelease = Release . normalizeRelease

-- | Normalize the release name.
normalizeRelease :: T.Text -> T.Text
normalizeRelease =
	normalizeName . fst . retrieveAuthor
	where
		splitProperly f = map T.strip . filter (not . T.null) . T.split f

		retrieveAuthor txt =
			case splitProperly (== '-') txt of
				[rn] -> (rn, Nothing)
				xs   -> (T.intercalate "-" (init xs),
				         Just (fst (T.span (not . isSpace) (last xs))))

		normalizeName =
			T.toLower . T.intercalate " " . splitProperly (not . isAlphaNum)

-- | Torrent
data Torrent = Torrent {
	torrentRelease     :: Release,
	torrentSource      :: [URI],
	torrentContentSize :: Maybe Word
} deriving (Show, Eq, Ord)

-- | Used to aggregate "Torrent"s.
newtype Aggregator = Aggregator { runAggregator :: Manager -> IO [Torrent] }

-- | "Monoid" instance which can be used to merge several "Aggregator"s.
instance Monoid Aggregator where
	mempty = Aggregator (const (pure []))
	mappend (Aggregator a) (Aggregator b) =
		Aggregator (\ mgr -> (++) <$> a mgr <*> b mgr)
	mconcat as = Aggregator (\ mgr -> fmap concat (mapM (\ (Aggregator f) -> f mgr) as))

-- | Fetch "Torrent"s.
fetch :: Aggregator -> IO [Torrent]
fetch = withManager tlsManagerSettings . runAggregator
