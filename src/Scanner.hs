{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Data.Int
import           Data.Void

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

import           HRel.Sources
import           HRel.Feeds
import           HRel.Torrents
import           HRel.Monad

data ScannerError
	= SourceError SourceError
	| FeedError FeedError
	deriving (Show)

type HRelSource m o = HRelT ScannerError (ConduitM () o) m ()

type HRelSink i m r = ConduitM i Void m r

torrentSources :: (MonadCatch m, MonadResource m) => Manager -> HRelSource m [Torrent]
torrentSources mgr =
	withHRelT SourceError $
		mapM_ (\ src -> reportAndRecover (report src) (torrentSource mgr src))
		      [RARBG "https://rarbg.to/rssdd_magnet.php?category=41",
		       RARBG "https://rarbg.to/rssdd_magnet.php?category=48;44;45;42"]
	where
		report src err = do
			putStr "SourceError during "
			putStr (show src)
			putStr ": "
			print err

torrentSink :: (MonadIO m) => P.Connection -> HRelSink [Torrent] m ()
torrentSink db =
	C.mapM_ $ \ torrents -> liftIO $ do
		result <- runErrand db (insertTorrents torrents)
		case result of
			Left err -> print err
			Right n  -> putStrLn ("Inserted " ++ show n ++ " new torrents")

feedSources :: (MonadCatch m, MonadResource m) => Manager -> P.Connection -> HRelSource m (Int64, Feed)
feedSources mgr db = do
	feeds <- liftIO $ do
		result <- runErrand db listFeeds

		case result of
			Left err -> [] <$ print err
			Right xs -> pure xs

	withHRelT FeedError $ forM_ feeds $ \ feed@(fid, _, url) ->
		reportAndRecover (report feed) (feedSource mgr url) =$= C.map ((,) fid)

	where
		report feed err = do
			putStr "FeedError during "
			putStr (show feed)
			putStr ": "
			print err

feedSink :: (MonadIO m) => P.Connection -> HRelSink (Int64, Feed) m ()
feedSink db =
	C.mapM_ $ \ (fid, Feed title contents) -> liftIO $ do
		result <- runErrand db $ do
			optional (updateFeedTitle fid title)
			mapM_ (associateSuitableTorrents fid) contents

		case result of
			Left err -> print err
			Right _  -> pure ()

main :: IO ()
main = do
	db <- P.connectdb "user=hrel dbname=hrel"
	mgr <- newManager tlsManagerSettings

	run (torrentSources mgr $$ torrentSink db)
	run (feedSources mgr db $$ feedSink db)

	where
		run x = runResourceT (runExceptT x) >>= either print (\ _ -> pure ())
