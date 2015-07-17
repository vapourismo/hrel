{-# LANGUAGE OverloadedStrings #-}

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           HRel.Source.Feeds
import           HRel.Processing
import           HRel.Database

main :: IO ()
main = withDatabase $ \ db -> withManager tlsManagerSettings $ \ mgr -> do
	feeds <- runAction db (query_ "SELECT id, url FROM feeds")

	runConduit $
		mapM_ (\ (fid, url) -> fromRSSTitles mgr url =$= C.map ((,) fid)) feeds
			=$= trackReleases db
			=$= findTorrents mgr
			=$= trackTorrents db
