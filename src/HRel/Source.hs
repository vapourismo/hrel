module HRel.Source (
	-- * Types
	Torrent (..),
	Aggregator (..),

	-- * Aggregation
	fetch,
) where

import qualified Data.Text as T

import Network.URI
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- | Torrent
data Torrent = Torrent {
	torrentRelease     :: T.Text,
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
