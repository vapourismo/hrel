{-# LANGUAGE OverloadedStrings #-}

module HRel.XML (
	Node (..),
	toNodeList
) where

import Data.Monoid
import Text.HTML.TagSoup

data Node t = Node { nodeTag        :: t
                   , nodeAttributes :: [Attribute t]
                   , nodeContents   :: t
                   , nodeChildren   :: [Node t] }

-- | Append text to a node's contents
appendText :: Monoid t => t -> Node t -> Node t
appendText ts (Node n a t cs) = Node n a (mappend t ts) cs

-- | Add a node to another node's children
appendNode :: Node t -> Node t -> Node t
appendNode c (Node n a t cs) = Node n a t (cs ++ [c])

-- | Build tree using the given XML segments
buildTree :: (Monoid t, Eq t) => [Node t] -> [Tag t] -> [Node t]
buildTree ns (TagOpen name attrs : ts) =
	-- Open a new node
	buildTree (Node name attrs mempty [] : ns) ts
buildTree (a : b : ns) (TagClose name : ts)
	| nodeTag a == name =
		-- The open node has been closed properly
		buildTree (appendNode a b : ns) ts
	| otherwise          =
		-- Skip invalid closing tags
		buildTree (a : b : ns) ts
buildTree (a : ns) (TagText txt : ts) =
	-- Append the text to the node contents
	buildTree (appendText txt a : ns) ts
buildTree ns (_ : ts) =
	-- Skip other elements
	buildTree ns ts
buildTree ns [] = ns

-- | Reformat the given list of "Tag"s into a list of "Node"s.
toNodeList :: (Monoid t, Eq t) => [Tag t] -> [Node t]
toNodeList = buildTree []
