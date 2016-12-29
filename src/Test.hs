{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric #-}

-- import           GHC.Generics

import           Control.Monad

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- import qualified Database.PostgreSQL.LibPQ as P
-- import           Database.PostgreSQL.Store

import           HRel.Markup
import           HRel.NodeFilter

-- leftOverToError :: T.Text -> T.Text -> (Int, T.Text)
-- leftOverToError input leftOver =
-- 	(T.length lineConsumed, T.append lineConsumed lineLeftOver)
-- 	where
-- 		consumed =
-- 			T.take (T.length input - T.length leftOver) input

-- 		lineConsumed =
-- 			T.takeWhileEnd (/= '\n') (T.takeEnd 50 consumed)

-- 		lineLeftOver =
-- 			T.takeWhile (/= '\n') (T.take 50 leftOver)

-- displayErrorLine :: T.Text -> T.Text -> IO ()
-- displayErrorLine input rest = do
-- 	let (cur, line) = leftOverToError input rest
-- 	T.putStrLn line
-- 	T.putStr (T.replicate cur " ")
-- 	T.putStrLn "^"

dumpFilter :: NodeFilter [(T.Text, T.Text)]
dumpFilter =
	"torrent" $//
		(,) <$> ("title" $/ text)
		    <*> ("magnet" $/ text)

-- showNode :: Int -> Node -> String
-- showNode n (Element name _ contents) =
-- 	replicate n '\t'
-- 	++ T.unpack name
-- 	++ "\n" ++ intercalate "\n" (map (showNode (n + 1)) contents)
-- showNode n (Text _) = replicate n '\t' ++ "#"

-- newtype Torrent = Torrent (T.Text, T.Text)
-- 	deriving (Generic)

-- instance Entity Torrent

-- anyColumnType :: ColumnType
-- anyColumnType =
-- 	ColumnType "blob" True Nothing

-- instance TableEntity Torrent where
-- 	describeTableType _ =
-- 		Table "tpb_dump"
-- 		      [Column "title" anyColumnType,
-- 		       Column "magnet" anyColumnType]

chunkify :: Int -> [a] -> [[a]]
chunkify _ [] = []
chunkify n xs = take n xs : chunkify n (drop n xs)

main :: IO ()
main = do
	-- db <- P.connectdb "user=hrel dbname=hrel"
	input <- T.readFile "/data/downloads/tpb-head.xml"
	case (parseTextMarkup input >>= \ node -> runNodeFilter node dumpFilter) of
		Nothing ->
			putStrLn "Failed"

		Just torrents ->
			forM_ torrents print
			-- forM_ (chunkify 100 torrents) $ \ ts -> do
			-- 	r <- runErrand db (insertMany (map Torrent ts))
			-- 	either print print r
