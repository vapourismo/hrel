module HRel.URI (
	toURI,
	toHost,

	module Network.URI
) where

import Control.Monad.Trans.Maybe
import Text.StringLike
import Network.URI

-- | Parse a string-like type.
toURI :: (Monad m, StringLike s) => s -> MaybeT m URI
toURI = MaybeT . return . parseURI . toString

-- | Get the host part of a URI.
toHost :: (Monad m) => URI -> MaybeT m String
toHost = MaybeT . return . fmap uriRegName . uriAuthority
