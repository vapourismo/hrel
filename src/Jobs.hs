{-# LANGUAGE OverloadedStrings #-}

import           Data.Conduit

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           HRel.Processing
import           HRel.JobControl
import           HRel.Database

main :: IO ()
main = do
	withDatabase $ \ db ->
		withManager tlsManagerSettings $ \ mgr ->
			withJobControl $ \ ctl ->
				runConduit $
					sourceFeeds db mgr ctl
						=$= trackReleases db
						=$= findTorrents mgr
						=$= trackTorrents db
