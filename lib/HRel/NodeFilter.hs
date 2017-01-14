module HRel.NodeFilter (
	NodeFilter,
	runNodeFilter,
	filterNodes,
	forNodes,
	forNode,
	forTags,
	forTag,
	forRelativeTag,
	attributes,
	attribute,
	text,
	($/),
	($//)
) where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Conduit
import           Data.Conduit.List (mapMaybe)

import           Data.List
import           Data.Foldable
import           Data.Maybe (catMaybes)

import qualified Data.Text as T

import           HRel.Markup

-- | Node filter
type NodeFilter = ReaderT Node Maybe

-- | Filter a given 'Node'.
runNodeFilter :: NodeFilter a -> Node -> Maybe a
runNodeFilter = runReaderT

-- | Run a 'NodeFilter' on incoming
filterNodes :: (Monad m) => NodeFilter a -> Conduit Node m a
filterNodes nf =
	mapMaybe (runNodeFilter nf)

-- | Traverse the sequence to transform certain nodes that match the given criteria.
mapFilterCat :: (Node -> Bool) -> (Node -> Maybe b) -> [Content] -> [b]
mapFilterCat cond fun contents =
	catMaybes (map fun' (filter cond' contents))
	where
		fun' (Nested node) = fun node
		fun' _             = Nothing

		cond' (Nested node) = cond node
		cond' _             = False

-- | Do something for nodes which match a certain predicate.
forNodes :: (Node -> Bool) -> NodeFilter a -> NodeFilter [a]
forNodes cond (ReaderT fun) =
	ReaderT (\ (Node _ _ contents) -> pure (mapFilterCat cond fun contents))

-- | Same as 'forNodes' but for a single node.
forNode :: (Node -> Bool) -> NodeFilter a -> NodeFilter a
forNode cond (ReaderT fun) =
	ReaderT $ \ (Node _ _ contents) -> do
		Nested elem <- find cond' contents
		fun elem
	where
		cond' (Nested node) = cond node
		cond' _             = False

-- | Do something for nodes with a certain name.
forTags :: T.Text -> NodeFilter a -> NodeFilter [a]
forTags tag =
	forNodes cond
	where
		cond (Node name _ _) = name == tag

-- | Same as 'forTags' but for a single node.
forTag :: T.Text -> NodeFilter a -> NodeFilter a
forTag tag =
	forNode cond
	where
		cond (Node name _ _) = name == tag

-- | Find a node that matches the given criteria. Searches the tree recursively.
deepFind :: (Node -> Bool) -> [Content] -> Maybe Node
deepFind cond =
	finder
	where
		finder nodes =
			case find cond' nodes of
				Just (Nested x) -> Just x
				_               -> nestSearch nodes

		cond' (Nested node) = cond node
		cond' _             = False

		nestSearch [] = Nothing
		nestSearch (Nested (Node _ _ otherNodes) : xs) =
			finder (toList otherNodes) <|> nestSearch xs
		nestSearch (_ : xs) = nestSearch xs

-- | Do something for a node with a certain tag within the entire tree.
forRelativeTag :: T.Text -> NodeFilter a -> NodeFilter a
forRelativeTag name (ReaderT fun) =
	ReaderT $ \ (Node _ _ contents) -> deepFind cond (toList contents) >>= fun
	where
		cond (Node tag _ _) = tag == name

-- | Retrieve the current nodes attributes.
attributes :: NodeFilter [(T.Text, T.Text)]
attributes =
	ReaderT (\ (Node _ a _) -> pure a)

-- | Get the value of an attribute.
attribute :: T.Text -> NodeFilter T.Text
attribute name = do
	attrs <- attributes
	lift (lookup name attrs)

-- | Gather the entire text of the current node.
text :: NodeFilter T.Text
text =
	asks fromNode
	where
		fromContent (Text t)   = t
		fromContent (Nested n) = fromNode n

		fromNode (Node _ _ contents) =
			T.concat (map fromContent (toList contents))


infixr 0 $/

-- | Alias for 'forTag'.
($/) :: T.Text -> NodeFilter a -> NodeFilter a
($/) = forTag

infixr 0 $//

-- | Alias for 'forTags'.
($//) :: T.Text -> NodeFilter a -> NodeFilter [a]
($//) = forTags
