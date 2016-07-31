{-# LANGUAGE OverloadedStrings, TemplateHaskell	, QuasiQuotes #-}

module Main where

import           Control.Monad.Except

import           HRel.Network
import           HRel.Models
import           HRel.Sources

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified Database.PostgreSQL.LibPQ as P
import           Database.PostgreSQL.Store

insert' :: (Table a) => a -> Errand (Maybe (Reference a))
insert' x =
	catchError (Just <$> insert x) (\ _ -> pure Nothing)

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings
	db <- P.connectdb "postgres://hrel@localhost/hrel"

	runErrand db $
		query_ $(mkCreateQuery ''Torrent)

	Just torrents <- downloadMarkup mgr "https://thepiratebay.org/rss/top100/0" pirateBaySource
	Right res <- runErrand db (mapM insert' torrents)

	print res
