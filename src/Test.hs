{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Control.Monad.Except
import           Control.Monad.Catch

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           HRel.Sources
import           HRel.Torrents
import           HRel.Monad

type HRelSource m o = HRelT SourceError (ConduitM () o) m ()

sources :: (MonadCatch m, MonadResource m) => Manager -> HRelSource m [Torrent]
sources mgr =
	mapM_ (torrentSource mgr)
	      [RARBG "https://rarbg.to/rssdd_magnet.php?category=41",
	       RARBG "https://rarbg.to/rssdd_magnet.php?category=48;44;45;42"]

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings
	result <- runResourceT $ runExceptT $ runConduit $
		sources mgr =$= C.mapM_ (liftIO . mapM_ print)

	either print (const (pure ())) result
