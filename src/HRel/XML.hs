{-# LANGUAGE OverloadedStrings #-}

module HRel.XML (
	Node (..),
	toNodeList,

	NodeFilter,
	runNodeFilter,
	foreachNode,
	node,
	attribute,
	text,
) where

import Data.List
import Data.Monoid

import Control.Monad.Trans.Maybe
import Control.Monad.Reader

import Text.HTML.TagSoup

-- | Node data type
data Node t
	= Tag t [Attribute t] [Node t]
	| Text t

-- | Is the given "Node" a tag with the given tag name?
isNodeTag :: (Eq t) => t -> Node t -> Bool
isNodeTag t (Tag t' _ _) = t == t'
isNodeTag _ _ = False

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

-- | Cursor used to navigate through an XML document
type NodeFilter t = MaybeT (Reader (Node t))

-- | Apply the filter to a node.
runNodeFilter :: NodeFilter t a -> Node t -> Maybe a
runNodeFilter a = runReader (runMaybeT a)

-- | Iterate through every "Node" matching the given tag name.
foreachNode :: (Eq t) => t -> NodeFilter t a -> NodeFilter t [a]
foreachNode t a =
	asks (filter (isNodeTag t) . nodeChildren)
	>>= mapM (\child -> local (const child) a)

-- | Apply the filter to a sub-node with the given tag name.
node :: (Eq t) => t -> NodeFilter t a -> NodeFilter t a
node t a =
	MaybeT (asks (find (isNodeTag t) . nodeChildren))
	>>= \n -> local (const n) a

-- | Fetch value of an attribute.
attribute :: (Eq t) => t -> NodeFilter t t
attribute k = MaybeT (fmap (lookup k) (asks nodeAttributes))

-- | Get the inner content.
text :: (Monoid t) => NodeFilter t t
text = asks nodeText
