module HRel.Tools (
	-- * URI Tools
	toURI,
	toHost,
	validHosters,
	isValidHost,

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

-- | Allow only these hosters
validHosters :: [String]
validHosters = ["ul.to", "uploaded.to", "uploaded.net"]

-- | Check if the given host name maybe be used.
isValidHost :: String -> Bool
isValidHost = flip elem validHosters

-- | Trim space around a text.
trimText :: T.Text -> T.Text
trimText = T.dropAround isSpace
