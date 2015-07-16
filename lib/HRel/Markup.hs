{-# LANGUAGE OverloadedStrings #-}

module HRel.Markup (
	-- * Parsing
	Node (..),
	toNodeList,
	fromMarkup,
	fromMarkup',

	-- * Filtering
	NodeFilterT,
	NodeFilter,
	runNodeFilterT,
	runNodeFilter,

	-- * Navigation combinators
	foreachNode,
	forNode,
	foreachTag,
	forTag,
	relativeNode,
	relativeNodes,
	relativeTag,
	relativeTags,

	-- * Property getters
	tagName,
	attr,
	text,

	-- * Property guards
	attrGuard
) where

import           Control.Applicative       hiding (empty)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

import           Data.List
import           Data.Maybe
import           Data.Functor.Identity

import           Text.HTML.TagSoup
import           Text.StringLike

-- | Node data type
data Node t
	= Tag t [Attribute t] [Node t]
	| Text t

-- | Get all attributes.
nodeAttributes :: Node t -> [Attribute t]
nodeAttributes (Tag _ attrs _) = attrs
nodeAttributes _ = []

-- | Get all children.
nodeChildren :: Node t -> [Node t]
nodeChildren (Tag _ _ cs) = cs
nodeChildren _ = []

-- | Get the underlying text to the given node.
nodeText :: (Monoid t) => Node t -> t
nodeText (Text t) = t
nodeText (Tag _ _ cs) = mconcat (map nodeText cs)

-- | Show function for "Node"
showNode :: (Show t) => Int -> Node t -> String
showNode i n =
	indent ++ contents
	where
		showAttr (name, val) =
			"\n" ++ indent ++ ">      "
			++ show name ++ " = " ++ show val
		contents =
			case n of
				Tag tag attrs [] ->
					"> tag: " ++ show tag
					++ concatMap showAttr attrs
				Tag tag attrs cs ->
					"> tag: " ++ show tag
					++ concatMap showAttr attrs ++ "\n"
					++ intercalate "\n" (map (showNode (i + 1)) cs)
				Text txt -> "> text: " ++ show txt
		indent = concat (replicate i "    ")

instance (Show t) => Show (Node t) where
	show = showNode 0

-- | Generate a single node from the given tag list.
build :: (Eq t) => [Tag t] -> Maybe (Node t, [Tag t])
build [] = Nothing
build (TagText txt : tags) = Just (Text txt, tags)
build (TagOpen tag attrs : ts) =
	Just (Tag tag attrs (fromChildren children), rest)
	where
		(children, rest) = findChildren (0 :: Int) [] ts

		-- Find tags that qualify as children
		findChildren 0 stack (TagClose tag' : ts')
			| tag' == tag = (reverse stack, ts')
			| otherwise = (reverse stack, ts)
		findChildren _ stack [] = (reverse stack, [])
		findChildren n stack (TagClose tag' : ts') =
			findChildren (n - 1) (TagClose tag' : stack) ts'
		findChildren n stack (TagOpen tag' attrs' : ts') =
			findChildren (n + 1) (TagOpen tag' attrs' : stack) ts'
		findChildren n stack (TagText txt : ts') =
			findChildren n (TagText txt : stack) ts'
		findChildren n stack (_ : ts') =
			findChildren n stack ts'

		-- Generate the node children from the given tag list
		fromChildren cs = maybe [] (\(n, cs') -> n : fromChildren cs') (build cs)
build (_ : tags) = build tags

-- | Reformat the given tag list into a node list.
toNodeList :: (Eq t) => [Tag t] -> [Node t]
toNodeList ts =
	case build ts of
		Just (n, ts') -> n : toNodeList ts'
		Nothing -> []

-- | Parse the input.
fromMarkup :: (StringLike t) => t -> [Node t]
fromMarkup = toNodeList . parseTags

-- | Parse the input
fromMarkup' :: (StringLike t) => t -> Node t
fromMarkup' ts =
	case fromMarkup ts of
		[x] -> x
		xs  -> Tag empty [] xs

-- | Cursor used to navigate through an XML document
type NodeFilterT t m = MaybeT (ReaderT (Node t) m)

-- | Apply the filter to a node.
runNodeFilterT :: NodeFilterT t m a -> Node t -> m (Maybe a)
runNodeFilterT a = runReaderT (runMaybeT a)

-- | Shortcut for "NodeFilterT" in an "Identity" monad
type NodeFilter t = NodeFilterT t Identity

-- | Apply the filter to a node.
runNodeFilter :: NodeFilter t a -> Node t -> Maybe a
runNodeFilter a = runReader (runMaybeT a)

-- | Match any child node.
foreachNode :: (Monad m, Functor m) => NodeFilterT t m a -> NodeFilterT t m [a]
foreachNode a =
	reader nodeChildren
	>>= fmap catMaybes . mapM (\n -> local (const n) (optional a))

-- | Match one child node.
forNode :: (Monad m, Functor m) => NodeFilterT t m a -> NodeFilterT t m a
forNode a = do
	cs <- reader nodeChildren
	case cs of
		[] -> mzero
		(y : ys) -> foldl' (\x n -> x <|> sub n a) (sub y a) ys
	where
		sub = local . const

-- | Like "foreachNode" but for "Tag"s.
foreachTag :: (Eq t, Monad m, Functor m) => t -> NodeFilterT t m a -> NodeFilterT t m [a]
foreachTag t a =
	foreachNode (tagName >>= guard . (== t) >> a)

-- | Like "forNode" but for "Tag"s.
forTag :: (Eq t, Monad m, Functor m) => t -> NodeFilterT t m a -> NodeFilterT t m a
forTag t a =
	forNode (tagName >>= guard . (== t) >> a)

-- | Match the current node or a node at a lower level.
relativeNode :: (Monad m, Functor m) => NodeFilterT t m a -> NodeFilterT t m a
relativeNode a = a <|> forNode (relativeNode a)

-- | Match the current and any node at a lower level.
relativeNodes :: (Monad m, Functor m) => NodeFilterT t m a -> NodeFilterT t m [a]
relativeNodes a =
	merge <$> optional a
	      <*> foreachNode (relativeNodes a)
	where
		merge h t = maybeToList h ++ concat t

-- | Like "relativeNode" but for "Tag"s.
relativeTag :: (Eq t, Monad m, Functor m) => t -> NodeFilterT t m a -> NodeFilterT t m a
relativeTag t a =
	relativeNode (tagName >>= guard . (== t) >> a)

-- | "Like relativeNodes" but for "Tag"s.
relativeTags :: (Eq t, Monad m, Functor m) => t -> NodeFilterT t m a -> NodeFilterT t m [a]
relativeTags t a =
	relativeNodes (tagName >>= guard . (== t) >> a)

-- | Fetch the tag name.
tagName :: (Monad m) => NodeFilterT t m t
tagName = MaybeT $ ask >>= \n ->
	return $ case n of
		Tag t _ _ -> return t
		_ -> mzero

-- | Fetch value of an attribute.
attr :: (Eq t, Monad m, Functor m) => t -> NodeFilterT t m t
attr k = MaybeT (fmap (lookup k) (reader nodeAttributes))

-- | Get the inner content.
text :: (Monoid t, Monad m) => NodeFilterT t m t
text = reader nodeText

-- | Check if an attribute is present and has a certain value.
attrGuard :: (Eq t, Monad m, Functor m) => t -> t -> NodeFilterT t m ()
attrGuard k v = attr k >>= guard . (== v)
