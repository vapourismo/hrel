module HRel.Aggregators (
	aggregate
) where

import qualified Data.Text.Lazy as T

import Network.URI

import qualified HRel.Aggregators.DDLValleyRocks as DDLValley
import qualified HRel.Aggregators.OneClickWatch  as OneClickWatch

-- | Aggregate links from every source.
aggregate :: IO [(,) [T.Text] [URI]]
aggregate =
	fmap concat $ sequence [DDLValley.aggregate,
	                        fmap (map fixOCW) OneClickWatch.aggregate]
	where
		fixOCW (t, u) = ([t], u)
