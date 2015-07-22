{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Word
import Network.HenServer
import HRel.Database

main :: IO ()
main = do
	con <- connect (localhost 3300)
	withDatabase $ \ db -> do
		feedIDs <- runAction db (query_ "SELECT id FROM feeds") :: IO [Only Word64]
		withConnection con $
			forM_ feedIDs (sendSerialized . fromOnly)
