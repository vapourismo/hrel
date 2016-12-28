{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           HRel.Parser
import           HRel.Parser.XML

leftOverToError :: T.Text -> T.Text -> (Int, T.Text)
leftOverToError input leftOver =
	(T.length lineConsumed, T.append lineConsumed lineLeftOver)
	where
		consumed =
			T.take (T.length input - T.length leftOver) input

		lineConsumed =
			T.takeWhileEnd (/= '\n') (T.takeEnd 30 consumed)

		lineLeftOver =
			T.takeWhile (/= '\n') (T.take 30 leftOver)

displayErrorLine :: T.Text -> T.Text -> IO ()
displayErrorLine input rest = do
	let (cur, line) = leftOverToError input rest
	T.putStrLn line
	T.putStr (T.replicate cur " ")
	T.putStrLn "^"

main :: IO ()
main = do
	input <- T.readFile "test.xml"
	case runParser xml input of
		(Nothing, rest) -> do
			putStrLn "Failed"
			displayErrorLine input rest

		(Just x, "") ->
			print x

		(_, rest) -> do
			putStrLn "Incomplete"
			displayErrorLine input rest
