{-# LANGUAGE OverloadedStrings #-}

--import           Data.Conduit

--import           Network.HTTP.Client
--import           Network.HTTP.Client.TLS

--import           HRel.Processing
--import           HRel.JobControl
--import           HRel.Database

--main :: IO ()
--main = do
--	withDatabase $ \ db ->
--		withManager tlsManagerSettings $ \ mgr ->
--			withJobControl $ \ ctl ->
--				runConduit $
--					sourceFeeds db mgr ctl
--						=$= trackReleases db
--						=$= findTorrents mgr
--						=$= trackTorrents db

import           Control.Monad
import           Control.Monad.Trans
import           Control.Concurrent

import           Data.Word
import           Data.Conduit
import qualified Data.Conduit.List   as C

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HenServer

import           HRel.Source.Feeds
import           HRel.Processing
import           HRel.Database
import           HRel.Release

sourceFeeds :: Database -> Manager -> Chan Word64 -> Source IO (Word64, Release)
sourceFeeds db mgr chan =
	forever $ do
		fid <- liftIO (readChan chan)
		mbURL <- runAction db (query "SELECT url FROM feeds WHERE id = ?" (Only fid))
		case mbURL of
			[Only url] -> do
				liftIO (putStrLn ("> Processing: " ++ url))
				toProducer (fromAtomTitles mgr url) =$= C.map ((,) fid)

			_ -> pure ()

main :: IO ()
main = do
	withDatabase $ \ db -> withManager tlsManagerSettings $ \ mgr -> do
		chan <- newChan

		forkIO $ serve (localhost 3300) $ forever $ do
			fid <- receiveSerialized
			liftIO (writeChan chan fid)

		runConduit $
			sourceFeeds db mgr chan
				=$= trackReleases db
				=$= findTorrents mgr
				=$= trackTorrents db
