module HRel.Units (
	showAsBytes
) where

-- | Prefix for byte units
data ByteUnitPrefix
	= NoBase2Suffix
	| Kibi
	| Mebi
	| Gibi
	| Tebi
	| Pebi
	| Exbi
	| Zebi
	| Yobi
	deriving (Eq, Ord, Enum)

-- | Show a number as bytes including unit.
showAsBytes :: Word -> String
showAsBytes n =
	makle n NoBase2Suffix
	where
		makle x p
			| x >= 1000 && p < Yobi =
				makle (div x 1000) (succ p)
			| otherwise =
				show x ++ case p of
					NoBase2Suffix -> " B"
					Kibi          -> " KiB"
					Mebi          -> " MiB"
					Gibi          -> " GiB"
					Tebi          -> " TiB"
					Pebi          -> " PiB"
					Exbi          -> " EiB"
					Zebi          -> " ZiB"
					Yobi          -> " YiB"
