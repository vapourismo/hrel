module HRel.Tools (
	-- * URI Tools
	toURI,
	toHost,

	-- * Text Tools
	trimText
) where

import Control.Monad.Trans.Maybe
import Data.Char
import qualified Data.Text.Lazy as T
import Text.StringLike
import Network.URI

-- | Parse a string-like type.
toURI :: (Monad m, StringLike s) => s -> MaybeT m URI
toURI = MaybeT . return . parseURI . toString

-- | Get the host part of a URI.
toHost :: (Monad m) => URI -> MaybeT m String
toHost = MaybeT . return . fmap uriRegName . uriAuthority

-- | Trim space around a text.
trimText :: T.Text -> T.Text
trimText = T.dropAround isSpace

