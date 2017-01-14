module HRel.Markup2 (
	Node (..),
	Content (..),
	buildNodes,
	unifyNodes
) where

import           Control.Monad.State.Strict

import           Data.Conduit
import qualified Data.Text     as T

import qualified HRel.XML      as X

-- | Markup node with its name, attributes and contents
data Node = Node T.Text [X.Attribute] [Content]
	deriving (Show, Eq, Ord)

-- | Node content
data Content
	= Nested Node
	| Text T.Text
	deriving (Show, Eq, Ord)

-- | Helper state transformer
type NodeBuilder m r = StateT [Node] (ConduitM X.Content Node m) r

-- |
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

-- | Close all open nodes, then yield the result.
finalizeStack :: (Monad m) => NodeBuilder m ()
finalizeStack = do
	s <- get
	lift (mapM_ yield (finalize s))
	where
		finalize (node : Node n a c : rest) = finalize (Node n a (c `snoc` Nested node) : rest)
		finalize x                          = x

-- | Push a new node onto the stack.
pushOpen :: T.Text -> [X.Attribute] -> [Node] -> [Node]
pushOpen n a rest = Node n a [] : rest

-- | Insert text into the top-most node.
pushText :: T.Text -> [Node] -> [Node]
pushText t (Node n a c : rest) = Node n a (c `snoc` Text t) : rest
pushText _ x                   = x

-- | Close the top-most node (if there is any).
closeNode :: (Monad m) => T.Text -> NodeBuilder m ()
closeNode name = do
	s <- get
	case s of
		[node1@(Node name1 _ _)] ->
			when (name == name1) $ do
				put []
				lift (yield node1)

		node1@(Node name1 _ _) : Node n2 a2 c2 : rest -> do
			put (Node n2 a2 (c2 `snoc` Nested node1) : rest)
			unless (name == name1) $
				closeNode name

		_ -> pure ()

-- | Do something with an instance of 'X.Content'.
interpretContent :: (Monad m) => X.Content -> NodeBuilder m ()
interpretContent c = do
	case c of
		X.Open n a  -> modify (pushOpen n a)
		X.Close n   -> closeNode n
		X.Text t    -> modify (pushText t)
		X.Empty n a -> modify (pushOpen n a) >> closeNode n
		_           -> pure ()

-- | Process inputs in order to produce 'Node's.
processContents :: (Monad m) => NodeBuilder m ()
processContents = do
	mbContent <- lift await
	case mbContent of
		Nothing ->
			finalizeStack

		Just c -> do
			interpretContent c
			processContents

-- | Build 'Node's with incomming 'X.Content's.
buildNodes :: (Monad m) => Conduit X.Content m Node
buildNodes =
	void (runStateT processContents [])

-- | Merge multiple 'Node's into one.
unifyNodes :: [Node] -> Maybe Node
unifyNodes []  = Nothing
unifyNodes [x] = Just x
unifyNodes xs  = Just (Node T.empty [] (map Nested xs))
