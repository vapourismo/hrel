module HRel.Database.Query
    ( Query (..)
    , toQuery
    )
where

import           Data.ByteString    (ByteString)
import qualified Data.Text.Encoding as Text

import qualified Data.ByteString.Char8 as ByteString

import HRel.Database.SQL.Builder
import HRel.Database.SQL.Select
import HRel.Database.Value

data Query i a = Query
    { queryCode   :: ByteString
    , queryParams :: i -> [Value]
    }

instance Show (Query i a) where
    show = ByteString.unpack . queryCode

toQuery :: Select i a -> Query i a
toQuery select =
    Query
        { queryCode   = Text.encodeUtf8 (unCode code)
        , queryParams = sequenceA params
        }
    where
        (params, code) = runBuilder (buildSelect select)
