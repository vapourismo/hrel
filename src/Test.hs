{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import           Control.Monad.Trans
import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           HRel.Sources2
import           HRel.Torrents

sources :: (MonadResource m) => Manager -> Producer m [Torrent]
sources mgr =
	mapM_ (torrentSource mgr)
	      [RARBG "https://rarbg.to/rssdd_magnet.php?category=41",
	       RARBG "https://rarbg.to/rssdd_magnet.php?category=48;44;45;42"]

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings
	runResourceT $ runConduit $
		sources mgr =$= C.mapM_ (liftIO . mapM_ print)
