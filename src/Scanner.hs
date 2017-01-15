{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Data.Void

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           HRel.Sources
import           HRel.Torrents
import           HRel.Names
import           HRel.Monad

reportSourceError :: TorrentSource -> SourceError -> IO ()
reportSourceError src err = do
	putStr "SourceError during "
	putStr (show src)
	putStr ": \n\t"
	print err

type HRelSource m o = HRelT SourceError (ConduitM () o) m ()

sources :: (MonadCatch m, MonadResource m) => Manager -> HRelSource m [Torrent]
sources mgr =
	mapM_ (\ src -> reportAndRecover (reportSourceError src) (torrentSource mgr src))
	      [RARBG "https://$rarbg.to/rssdd_magnet.php?category=41",
	       RARBG "https://$rarbg.to/rssdd_magnet.php?category=48;44;45;42"]

type HRelSink i m r = HRelT SourceError (ConduitM i Void) m r

sink :: (MonadResource m) => P.Connection -> HRelSink [Torrent] m ()
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
	result <- runResourceT $ runExceptT $ runConduit $
		sources mgr =$= sink db

	either print (const (pure ())) result
