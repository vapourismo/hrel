{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           HRel.Sources2
import           HRel.Torrents
import           HRel.Names

sources :: (MonadResource m) => Manager -> Source m [Torrent]
sources mgr =
	mapM_ (torrentSource mgr)
	      [RARBG "https://rarbg.to/rssdd_magnet.php?category=41",
	       RARBG "https://rarbg.to/rssdd_magnet.php?category=48;44;45;42"]

sink :: (MonadResource m) => P.Connection -> Sink [Torrent] m ()
sink db =
	C.mapM_ $ \ torrents -> liftIO $
		forM_ torrents $ \ torrent@(Torrent title _) -> do
			print title

			upsertionResult <- runErrand db $ do
				tid <- insertTorrent torrent
				associateTags tid (parseTags title)

			case upsertionResult of
				Left err -> print err
				_        -> pure ()

main :: IO ()
main = do
	db <- P.connectdb "user=hrel dbname=hrel"
	mgr <- newManager tlsManagerSettings
	runResourceT $ runConduit $
		sources mgr =$= sink db
