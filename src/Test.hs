{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import           Control.Monad.Except
import           Control.Monad.Trans.Resource

import           Data.Conduit
import qualified Data.Conduit.List as C

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           HRel.Feeds

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings
	result <- runResourceT $ runExceptT $ runConduit $
		feedSource mgr "https://www.xrel.to/releases-usrss.html?u=20470&s=ee663473a8da8a161902c908326ebe1c&favs=1"
		=$= C.consume

	print result
