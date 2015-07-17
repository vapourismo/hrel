{-# LANGUAGE OverloadedStrings #-}

module HRel.JobControl (
	-- * Connection
	withJobControl,

	-- * Action
	sourceChannel,
	sourceFeeds,

	-- * Re-export stuff
	JobControl
) where

import           Control.Monad.Trans
import           Control.Monad.Catch
import           Control.Concurrent    hiding (yield)

import           Data.Word
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.List     as C
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC

import qualified Database.Redis        as R

import           Network.HTTP.Client

import           HRel.Source.Feeds
import           HRel.Database
import           HRel.Release

-- | Connection to a Redis server
type JobControl = R.Connection

withJobControl :: (JobControl -> IO a) -> IO a
withJobControl a =
	connectJobControl >>= a
	where
		connectJobControl =
			R.connect R.defaultConnectInfo

sourceChannel :: (MonadIO m) => JobControl -> [B.ByteString] -> Source m (B.ByteString, B.ByteString)
sourceChannel con channels = do
	chan <- liftIO $ do
		chan <- newChan
		forkIO $ do
			R.runRedis con $ R.pubSub (R.subscribe channels) $ \ msg -> do
				writeChan chan (Just (R.msgChannel msg, R.msgMessage msg))
				pure mempty
			writeChan chan Nothing

		pure chan

	let loop = do
		mbMsg <- liftIO (readChan chan)
		case mbMsg of
			Just msg -> yield msg >> loop
			Nothing  -> pure ()

	loop

sourceFeeds :: (MonadThrow m, MonadIO m) => Database -> Manager -> JobControl -> Source m (Word64, Release)
sourceFeeds db mgr con =
	sourceChannel con ["hrel_feeds"] =$= fetchReleases
	where
		fetchReleases = do
			mbPacket <- await
			case mbPacket of
				Nothing ->
					pure ()

				Just ("hrel_feeds", bcfid) | BC.all isDigit bcfid -> do
					let fid = read (BC.unpack bcfid) :: Word64
					mbURL <- runAction db (query "SELECT url FROM feeds WHERE id = ?" (Only fid))
					case mbURL of
						[Only url] -> do
							liftIO (putStrLn ("> Processing: " ++ url))
							toProducer (fromRSSTitles mgr url) =$= C.map ((,) fid)

						_ ->
							pure ()

					fetchReleases

				Just _ ->
					fetchReleases
