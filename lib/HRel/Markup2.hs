module HRel.Markup2 (
	Node (..),
	Content (..),
	buildNodes,
	unifyNodes
) where

import           Control.Monad.State.Strict

import           Data.Conduit
import qualified Data.Text     as T
import qualified Data.Sequence as S

import qualified HRel.XML      as X

data Node = Node T.Text [X.Attribute] (S.Seq Content)
	deriving (Show, Eq, Ord)

data Content
	= Nested Node
	| Text T.Text
	deriving (Show, Eq, Ord)

type M m r = StateT [Node] (ConduitM X.Content Node m) r

finalizeStack :: (Monad m) => M m ()
finalizeStack = do
	s <- get
	lift (mapM_ yield (finalize s))
	where
		finalize (node : Node n a c : rest) = finalize (Node n a (c S.|> Nested node) : rest)
		finalize x                          = x

pushOpen :: T.Text -> [X.Attribute] -> [Node] -> [Node]
pushOpen n a rest = Node n a S.empty : rest

pushText :: T.Text -> [Node] -> [Node]
pushText t (Node n a c : rest) = Node n a (c S.|> Text t) : rest
pushText _ x                   = x

closeNode :: (Monad m) => T.Text -> M m ()
closeNode name = do
	s <- get
	case s of
		[node1@(Node name1 _ _)] ->
			when (name == name1) $ do
				put []
				lift (yield node1)

		node1@(Node name1 _ _) : Node n2 a2 c2 : rest -> do
			put (Node n2 a2 (c2 S.|> Nested node1) : rest)
			unless (name == name1) $
				closeNode name

		_ -> pure ()

interpretContent :: (Monad m) => X.Content -> M m ()
interpretContent c = do
	case c of
		X.Open n a  -> modify (pushOpen n a)
		X.Close n   -> closeNode n
		X.Text t    -> modify (pushText t)
		X.Empty n a -> modify (pushOpen n a) >> closeNode n
		_           -> pure ()

processContents :: (Monad m) => M m ()
processContents = do
	mbContent <- lift await
	case mbContent of
		Nothing ->
			finalizeStack

		Just c -> do
			interpretContent c
			processContents

buildNodes :: (Monad m) => Conduit X.Content m Node
buildNodes =
	void (runStateT processContents [])

unifyNodes :: [Node] -> Maybe Node
unifyNodes []  = Nothing
unifyNodes [x] = Just x
unifyNodes xs  = Just (Node T.empty [] (S.fromList (map Nested xs)))
