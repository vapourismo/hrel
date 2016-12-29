module HRel.Markup (
	Node (..),

	parseMarkup,
	parseMarkup_
) where

import           Control.Monad.Trans.Maybe

import qualified Data.ByteString as B

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           HRel.Parser
import qualified HRel.Parser.XML as X

-- | Temporary instantiation of a 'Node' inside a markup source
data TNode = TNode T.Text [X.Attribute] [Content]
	deriving (Show, Eq, Ord)

-- | Content of a 'TNode'
data Content
	= ContentNode TNode
	| ContentText T.Text
	deriving (Show, Eq, Ord)

-- | Traverse the 'Tag's in order to produce a 'TNode' tree.
traverseTags :: [X.Content] -> [TNode] -> [TNode]
traverseTags [] stack =
	case stack of
		[] -> []
		[x] -> [x]
		(node : TNode name attrs content : restNodes) ->
			traverseTags [] (TNode name attrs (content ++ [ContentNode node]) : restNodes)

traverseTags (tag : restTags) stack =
	case tag of
		X.Open name attrs ->
			traverseTags restTags (TNode name attrs [] : stack)

		X.Close _ ->
			case stack of
				[] -> traverseTags restTags stack
				[x] -> x : traverseTags restTags []
				(node : TNode name attrs content : restNodes) ->
					traverseTags restTags (TNode name attrs (content ++ [ContentNode node])
					                       : restNodes)

		X.Text text ->
			case stack of
				[] -> traverseTags restTags stack
				(TNode name attrs content : restNodes) ->
					traverseTags restTags (TNode name attrs (content ++ [ContentText text])
					                       : restNodes)

		X.Empty name attrs ->
			traverseTags (X.Open name attrs : X.Close name : restTags) stack

		_ -> traverseTags restTags stack

-- | Node inside the markup
data Node
	= Element T.Text [X.Attribute] [Node]
	| Text T.Text
	deriving (Show, Eq, Ord)

-- | Transform 'TNode' to 'Node'.
transformTNode :: TNode -> Node
transformTNode (TNode n a c) = Element n a (map transformContent c)

-- | Transform 'Content' to 'Node'.
transformContent :: Content -> Node
transformContent (ContentNode n) = transformTNode n
transformContent (ContentText t) = Text t

-- |
parseTags :: T.Text -> Maybe [X.Content]
parseTags input =
	fst (runParser X.xml input)

-- | Parse a given markup input and transform it into a list of 'Node's.
collectNodes :: T.Text -> Maybe [Node]
collectNodes source = do
	(\ contents -> map transformTNode (traverseTags contents [])) <$> parseTags source

-- | Parse a give markup input that contains only a single root 'Node'.
parseMarkup :: B.ByteString -> Maybe Node
parseMarkup input = do
	source <- either (const Nothing) Just (T.decodeUtf8' input)
	nodes <- collectNodes source
	case nodes of
		[]  -> Nothing
		[x] -> Just x
		xs  -> Just (Element T.empty [] xs)

-- |
parseMarkup_ :: (Applicative m) => B.ByteString -> MaybeT m Node
parseMarkup_ =
	MaybeT . pure . parseMarkup
