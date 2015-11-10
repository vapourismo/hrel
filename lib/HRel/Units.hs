module HRel.Units (
	showAsBytes
) where

-- | Prefix for byte units
data ByteSuffix
	= B
	| KiB
	| MiB
	| GiB
	| TiB
	| PiB
	| EiB
	| ZiB
	| YiB
	deriving (Show, Eq, Ord, Enum)

-- | Show a number as bytes including unit.
showAsBytes :: (Integral a) => a -> String
showAsBytes n =
	makle (fromIntegral n :: Float) B
	where
		makle x p
			| x >= 1000 && p < YiB =
				makle (x / 1000) (succ p)
			| otherwise =
				show (fromInteger (round (x * 100)) / 100 :: Float) ++ ' ' : show p
