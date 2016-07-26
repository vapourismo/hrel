module Main where

import HRel.Markup
import HRel.NodeFilter

test :: (Monad m) => NodeFilterT String m (String, [String])
test =
	forTag "feed" $ do
		(,) <$> forTag "title" text
		    <*> forTags "entry" (forTag "title" text)

main :: IO ()
main = do
	input <- readFile "test.xml"
	let [node] = parseMarkup input
	r <- runNodeFilterT test node
	print r
