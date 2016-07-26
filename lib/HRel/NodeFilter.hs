module HRel.NodeFilter where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.List
import Data.Maybe

import HRel.Markup

-- | Node filter
type NodeFilterT t m = ReaderT (Node t) (MaybeT m)

-- | Execute the node filter on a given 'Node'.
runNodeFilterT :: NodeFilterT t m a -> Node t -> m (Maybe a)
runNodeFilterT f n = runMaybeT (runReaderT f n)

collectSuccesful :: (Monad m) => [MaybeT m a] -> MaybeT m [a]
collectSuccesful nfs =
	lift (catMaybes <$> sequence (map runMaybeT nfs))

-- | Do something for 'Node's which match the criteria.
forNodes :: (Monad m) => (Node t -> Bool) -> NodeFilterT t m a -> NodeFilterT t m [a]
forNodes cond (ReaderT fun) = do
	ReaderT $ \ node ->
		case node of
			Element _ _ contents -> collectSuccesful (map fun (filter cond contents))
			_                    -> mzero

-- | Do something for a first 'Node' that matches the criteria.
forNode :: (Monad m) => (Node t -> Bool) -> NodeFilterT t m a -> NodeFilterT t m a
forNode cond (ReaderT fun) = do
	ReaderT $ \ node ->
		case node of
			Element _ _ contents -> MaybeT (pure (find cond contents)) >>= fun
			_                    -> mzero

-- | Do something for all 'Node's which are elements and match the criteria.
forElements :: (Monad m)
            => (t -> [Attribute t] -> [Node t] -> Bool)
            -> NodeFilterT t m a
            -> NodeFilterT t m [a]
forElements cond =
	forNodes $ \ node ->
		case node of
			Element n a c -> cond n a c
			_             -> False

-- | Do something for the first 'Node' which is an element and matches the criteria.
forElement :: (Monad m)
           => (t -> [Attribute t] -> [Node t] -> Bool)
           -> NodeFilterT t m a
           -> NodeFilterT t m a
forElement cond =
	forNode $ \ node ->
		case node of
			Element n a c -> cond n a c
			_             -> False

-- | Do something for each text node.
forTexts :: (Monad m) => NodeFilterT t m a -> NodeFilterT t m [a]
forTexts =
	forNodes $ \ node ->
		case node of
			Text _ -> True
			_      -> False

-- | Do something for each 'Node' which is an element and matches the given tag name.
forTags :: (Monad m, Eq t) => t -> NodeFilterT t m a -> NodeFilterT t m [a]
forTags tag =
	forElements (\ name _ _ -> tag == name)

-- | Do something for the first 'Node' which is an element and matches the given tag name.
forTag :: (Monad m, Eq t) => t -> NodeFilterT t m a -> NodeFilterT t m a
forTag tag =
	forElement (\ name _ _ -> tag == name)

textContent :: (Monad m) => NodeFilterT t m t
textContent =
	ReaderT $ \ node ->
		case node of
			Text content -> pure content
			_            -> mzero

-- | Extract the attributes from the current 'Node'.
attributes :: (Monad m) => NodeFilterT t m [Attribute t]
attributes =
	ReaderT $ \ node ->
		case node of
			Element _ attrs _ -> pure attrs
			_                 -> mzero

-- | Extract the value of a specific attribute from the current 'Node'.
attribute :: (Monad m, Eq t) => t -> NodeFilterT t m t
attribute name = do
	attrs <- attributes
	lift (MaybeT (pure (lookup name attrs)))

-- | Extract the text from the current 'Node'.
text :: (Monad m, Monoid t) => NodeFilterT t m t
text = textContent <|> mconcat <$> forNodes (const True) text

infixr 0 $/

-- | Alias for 'forTag'.
($/) :: (Monad m, Eq t) => t -> NodeFilterT t m a -> NodeFilterT t m a
($/) = forTag

infixr 0 $//

-- | Alis for 'forTags'.
($//) :: (Monad m, Eq t) => t -> NodeFilterT t m a -> NodeFilterT t m [a]
($//) = forTags
