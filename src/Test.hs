{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import           Control.Monad.Trans.Resource

import qualified Data.Text as T

import           Data.Conduit
import qualified Data.Conduit.List as C
import           Data.Conduit.Text
import           Data.Conduit.Attoparsec

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import qualified HRel.XML as X
import           HRel.Markup2
import           HRel.NodeFilter2
import           HRel.Network2

type Torrent = (T.Text, T.Text)

rarbgSource :: NodeFilter [Torrent]
rarbgSource =
	"channel" $/ "item" $//
		buildTorrent <$> ("title" $/ text)
		             <*> ("link" $/ text)
	where
		buildTorrent title uri =
			(T.strip title, T.strip uri)

processNodes :: (Monad m) => NodeFilter [a] -> ConduitM Node a m ()
processNodes nf = do
	awaitForever $ \ node ->
		case runNodeFilter nf node of
			Just xs -> mapM_ yield xs
			Nothing -> pure ()

source :: (MonadResource m) => Manager -> String -> NodeFilter [Torrent] -> ConduitM i Torrent m ()
source mgr url nf =
	httpRequest mgr url
		=$= decode utf8
		=$= conduitParser X.content
		=$= C.map snd
		=$= buildNodes
		=$= processNodes nf

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings
	nodes <- runResourceT $ runConduit $
		(source mgr "https://rarbg.to/rssdd_magnet.php?category=41" rarbgSource
		 >> source mgr "https://rarbg.to/rssdd_magnet.php?category=48;44;45;42" rarbgSource)
			=$= C.consume

	mapM_ print nodes
	print (length nodes)

	-- case unifyNodes nodes >>= runNodeFilter rarbgSource of
	-- 	Nothing -> putStrLn "Nothing"
	-- 	Just xs -> mapM_ print xs
