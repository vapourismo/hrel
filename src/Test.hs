{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource

import qualified Data.Text    as T

import           Data.Attoparsec.Text

import           Data.Conduit
import qualified Data.Conduit.List as C
import           Data.Conduit.Text
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.Attoparsec

import           HRel.XML

type S m = StateT

main :: IO ()
main = do
	c <- runResourceT $ runConduit $
		sourceFile "/data/downloads/tpb-head.xml"
			=$= decode utf8
			=$= conduitParser content
			=$= C.map snd
			=$= C.consume

	mapM_ print c
