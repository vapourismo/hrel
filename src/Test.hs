{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans
import           Control.Monad.Trans.Resource

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Conduit
import qualified Data.Conduit.List as C
import           Data.Conduit.Text
import           Data.Conduit.Binary (sourceFile)

import qualified HRel.XML as X
import           HRel.Markup2
-- import           HRel.Markup
import           HRel.Parser
-- import           HRel.NodeFilter
import           HRel.NodeFilter2

dumpFilter :: NodeFilter [(T.Text, T.Text)]
dumpFilter =
	"torrent" $//
		(,) <$> ("title" $/ text)
		    <*> ("magnet" $/ text)

leftOverToError :: T.Text -> T.Text -> (Int, T.Text)
leftOverToError input leftOver =
	(T.length lineConsumed, T.append lineConsumed lineLeftOver)
	where
		consumed =
			T.take (T.length input - T.length leftOver) input

		lineConsumed =
			T.takeWhileEnd (/= '\n') (T.takeEnd 50 consumed)

		lineLeftOver =
			T.takeWhile (/= '\n') (T.take 50 leftOver)

displayErrorLine :: T.Text -> T.Text -> IO ()
displayErrorLine input rest = do
	let (cur, line) = leftOverToError input rest
	T.putStrLn line
	T.putStr (T.replicate cur " ")
	T.putStrLn "^"

-- formatInputNicely :: T.Text -> T.Text
-- formatInputNicely txt
-- 	| T.length txt > 203 = T.concat [T.take 100 txt, "...", T.takeEnd 100 txt]
-- 	| otherwise          = txt

conduitParser :: (MonadIO m, Show a) => Parser a -> ConduitM T.Text a m ()
conduitParser p = do
	mbInput <- await
	case mbInput of
		Just input -> handleResult (parse p input) input
		Nothing    -> pure ()
	where
		handleResult result input = do
			case result of
				Complete input' x -> do
					yield x

					if T.null input' then
						conduitParser p
					else
						handleResult (parse p input') input'

				Error rest ->
					liftIO $ do
						putStrLn "Error"
						displayErrorLine input rest

				Incomplete cont -> do
					continue cont

		continue cont = do
			mbInput <- await
			case mbInput of
				Just input -> handleResult (cont input) input
				Nothing    -> handleResult (cont T.empty) T.empty

main :: IO ()
main = do
	-- c <- T.readFile "/data/downloads/tpb-head.xml"
	-- case parseTextMarkup c >>= \ n -> runNodeFilter n dumpFilter of
	-- 	Nothing -> putStrLn "Nothing"
	-- 	Just xs -> mapM_ print xs

	nodes <- runResourceT $ runConduit $
		sourceFile "/data/downloads/tpb-head.xml"
			=$= decode utf8
			=$= conduitParser X.content
			=$= buildNodes
			=$= C.consume

	case unifyNodes nodes >>= runNodeFilter dumpFilter of
		Nothing -> putStrLn "Nothing"
		Just xs -> mapM_ print xs
