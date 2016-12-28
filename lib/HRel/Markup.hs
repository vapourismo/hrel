module HRel.Markup (
	Node (..),
	Attribute,

	parseMarkup_
) where

import qualified Data.ByteString as B

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Text.HTML.TagSoup

-- | Temporary instantiation of a 'Node' inside a markup source
data TNode a = TNode a [Attribute a] [Content a]
	deriving (Show, Eq, Ord)

-- | Content of a 'TNode'
data Content a
	= ContentNode (TNode a)
	| ContentText a
	deriving (Show, Eq, Ord)

-- | Traverse the 'Tag's in order to produce a 'TNode' tree.
traverseTags :: [Tag a] -> [TNode a] -> [TNode a]
traverseTags [] stack =
	case stack of
		[] -> []
		[x] -> [x]
		(node : TNode name attrs content : restNodes) ->
			traverseTags [] (TNode name attrs (content ++ [ContentNode node]) : restNodes)

traverseTags (tag : restTags) stack =
	case tag of
		TagOpen name attrs ->
			traverseTags restTags (TNode name attrs [] : stack)

		TagClose _ ->
			case stack of
				[] -> traverseTags restTags stack
				[x] -> x : traverseTags restTags []
				(node : TNode name attrs content : restNodes) ->
					traverseTags restTags (TNode name attrs (content ++ [ContentNode node])
					                       : restNodes)

		TagText text ->
			case stack of
				[] -> traverseTags restTags stack
				(TNode name attrs content : restNodes) ->
					traverseTags restTags (TNode name attrs (content ++ [ContentText text])
					                       : restNodes)

		_ -> traverseTags restTags stack

-- | Node inside the markup
data Node a
	= Element a [Attribute a] [Node a]
	| Text a
	deriving (Show, Eq, Ord)

-- | Transform 'TNode' to 'Node'.
transformTNode :: TNode a -> Node a
transformTNode (TNode n a c) = Element n a (map transformContent c)

-- | Transform 'Content' to 'Node'.
transformContent :: Content a -> Node a
transformContent (ContentNode n) = transformTNode n
transformContent (ContentText t) = Text t

-- | Parse a given markup input and transform it into a list of 'Node's.
collectNodes :: T.Text -> [Node T.Text]
collectNodes source =
	map transformTNode (traverseTags (parseTags source) [])

-- | Parse a give markup input that contains only a single root 'Node'.
parseMarkup_ :: B.ByteString -> Maybe (Node T.Text)
parseMarkup_ input = do
	source <- either (const Nothing) Just (T.decodeUtf8' input)
	case collectNodes source of
		[]  -> Nothing
		[x] -> Just x
		xs  -> Just (Element T.empty [] xs)
