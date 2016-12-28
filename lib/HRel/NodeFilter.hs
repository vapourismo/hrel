module HRel.NodeFilter (
	NodeFilter,
	runNodeFilter,
	runNodeFilter_,
	forNodes,
	forNode,
	forElements,
	forElement,
	forTags,
	forTag,
	forRelativeTag,
	forTexts,
	attributes,
	attribute,
	text,
	($/),
	($//)
) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe

import           Data.List
import           Data.Maybe
import qualified Data.Text as T

import           HRel.Markup

-- | Node filter which works inside the 'Identity' monad.
type NodeFilter = ReaderT Node Maybe

-- | Execute the node filter on a given 'Node'.
runNodeFilter :: Node -> NodeFilter a -> Maybe a
runNodeFilter n f =
	runReaderT f n

-- | Execute the node filter on a given 'Node'.
runNodeFilter_ :: (Applicative m) => Node -> NodeFilter a -> MaybeT m a
runNodeFilter_ n f =
	MaybeT (pure (runNodeFilter n f))

-- | Do something for 'Node's which match the criteria.
forNodes :: (Node -> Bool) -> NodeFilter a -> NodeFilter [a]
forNodes cond (ReaderT fun) = do
	ReaderT $ \ node ->
		case node of
			Element _ _ contents -> pure (catMaybes (map fun (filter cond contents)))
			_                    -> mzero

-- | Do something for a first 'Node' that matches the criteria.
forNode :: (Node -> Bool) -> NodeFilter a -> NodeFilter a
forNode cond (ReaderT fun) = do
	ReaderT $ \ node ->
		case node of
			Element _ _ contents -> find cond contents >>= fun
			_                    -> mzero

-- | Do something for all 'Node's which are elements and match the criteria.
forElements :: (T.Text -> [Attribute T.Text] -> [Node] -> Bool)
            -> NodeFilter a
            -> NodeFilter [a]
forElements cond =
	forNodes $ \ node ->
		case node of
			Element n a c -> cond n a c
			_             -> False

-- | Do something for the first 'Node' which is an element and matches the criteria.
forElement :: (T.Text -> [Attribute T.Text] -> [Node] -> Bool)
           -> NodeFilter a
           -> NodeFilter a
forElement cond =
	forNode $ \ node ->
		case node of
			Element n a c -> cond n a c
			_             -> False

-- | Do something for each text node.
forTexts :: NodeFilter a -> NodeFilter [a]
forTexts =
	forNodes $ \ node ->
		case node of
			Text _ -> True
			_      -> False

-- | Do something for each 'Node' which is an element and matches the given tag name.
forTags :: T.Text -> NodeFilter a -> NodeFilter [a]
forTags tag =
	forElements (\ name _ _ -> tag == name)

-- | Do something for the first 'Node' which is an element and matches the given tag name.
forTag :: T.Text -> NodeFilter a -> NodeFilter a
forTag tag =
	forElement (\ name _ _ -> tag == name)

-- |
deepFind :: (Node -> Bool) -> [Node] -> Maybe (Node)
deepFind cond nodes =
	case find cond nodes of
		Nothing | length nested > 0 -> deepFind cond nested
		x -> x
	where
		nested =
			concat $ flip map nodes $ \ node ->
				case node of
					Element _ _ contents -> contents
					_ -> []


-- | Do something for a relative tag (need not be in the current node).
forRelativeTag :: T.Text -> NodeFilter a -> NodeFilter a
forRelativeTag name (ReaderT fun) =
	ReaderT $ \ node ->
		case node of
			Element _ _ contents -> deepFind cond contents >>= fun
			_                    -> mzero
	where
		cond (Element tag _ _ ) = tag == name
		cond _                  = False

-- |
textContent :: NodeFilter T.Text
textContent =
	ReaderT $ \ node ->
		case node of
			Text content -> pure content
			_            -> mzero

-- | Extract the attributes from the current 'Node'.
attributes :: NodeFilter [Attribute T.Text]
attributes =
	ReaderT $ \ node ->
		case node of
			Element _ attrs _ -> pure attrs
			_                 -> mzero

-- | Extract the value of a specific attribute from the current 'Node'.
attribute :: T.Text -> NodeFilter T.Text
attribute name = do
	attrs <- attributes
	lift (lookup name attrs)

-- | Extract the text from the current 'Node'.
text :: NodeFilter T.Text
text = textContent <|> mconcat <$> forNodes (const True) text

infixr 0 $/

-- | Alias for 'forTag'.
($/) :: T.Text -> NodeFilter a -> NodeFilter a
($/) = forTag

infixr 0 $//

-- | Alis for 'forTags'.
($//) :: T.Text -> NodeFilter a -> NodeFilter [a]
($//) = forTags
