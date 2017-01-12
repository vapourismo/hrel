{-# LANGUAGE OverloadedStrings, RankNTypes #-}

import           Control.Monad.Trans.Resource

import qualified Data.Text as T

import           Data.Conduit
import qualified Data.Conduit.List as C
import           Data.Conduit.Text
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.Attoparsec

import qualified HRel.XML as X
import           HRel.Markup2
import           HRel.NodeFilter2

dumpFilter :: NodeFilter [(T.Text, T.Text)]
dumpFilter =
	"torrent" $//
		(,) <$> ("title" $/ text)
		    <*> ("magnet" $/ text)

main :: IO ()
main = do
	nodes <- runResourceT $ runConduit $
		sourceFile "/data/downloads/tpb-head.xml"
			=$= decode utf8
			=$= conduitParser X.content
			=$= C.map snd
			=$= buildNodes
			=$= C.consume

	case unifyNodes nodes >>= runNodeFilter dumpFilter of
		Nothing -> putStrLn "Nothing"
		Just xs -> mapM_ print xs

	putStrLn "parser-2-conduit"
