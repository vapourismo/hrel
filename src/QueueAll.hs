{-# LANGUAGE OverloadedStrings #-}

import           HRel.JobControl
import           HRel.Database

main :: IO ()
main = do
	withDatabase $ \ db ->
		withJobControl $ \ jctl -> do
			feedIDs <- runAction db (query_ "SELECT id FROM feeds")
			mapM_ (queueFeedProcess jctl . fromOnly) feedIDs
