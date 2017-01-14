{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import           Control.Monad.Trans
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

source :: (MonadResource m) => Manager -> String -> NodeFilter [Torrent] -> Conduit i m [Torrent]
source mgr url nf =
	httpRequest mgr url
		=$= decode utf8
		=$= conduitParser X.content
		=$= C.map snd
		=$= buildNodes
		=$= filterNodes nf

main :: IO ()
main = do
	mgr <- newManager tlsManagerSettings
	runResourceT $ runConduit $
		(source mgr "https://rarbg.to/rssdd_magnet.php?category=41" rarbgSource
		 >> source mgr "https://rarbg.to/rssdd_magnet.php?category=48;44;45;42" rarbgSource)
			=$= C.mapM_ (liftIO . mapM_ print)
