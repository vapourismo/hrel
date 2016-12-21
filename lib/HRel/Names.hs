{-# LANGUAGE OverloadedStrings #-}

module HRel.Names (
	parseTags,
	parseNameTags
) where

import           Data.Char
import           Data.List
import qualified Data.Text as T

-- | Strip the release group name.
stripGroup :: T.Text -> (T.Text, Maybe T.Text)
stripGroup fullName =
	case T.breakOnEnd "-" fullName of
		-- No suffix
		("", name) -> (name, Nothing)

		-- Has suffix
		(name, group) -> (T.strip (T.dropEnd 1 name), Just (T.strip group))

-- | Split the text into alphanumeric segments.
splitSegments :: T.Text -> [T.Text]
splitSegments name =
	filter (not . T.null) (T.split (not . isAlphaNum) (T.toLower name))

-- | List of release qualifiers.
releaseQualifiers :: [T.Text]
releaseQualifiers =
	["1080p", "6ch", "720p", "aac", "aac2", "ac3", "bdrip", "bluray", "brrip", "cam", "dl", "dts",
	 "dvdrip", "dvdscr", "extended", "fansub", "h264", "hc", "hd", "hdcam", "hdrip", "hdtc", "hdts",
	 "hdtv", "hevc", "hq", "internal", "ppv", "repack", "rip", "tc", "telesync", "ts", "web",
	 "webrip", "x264", "x265", "xv", "xvid"]

-- | Retrieve tags within the name without sorting out the qualifiers.
parseTags :: T.Text -> [T.Text]
parseTags =
	splitSegments . fst . stripGroup

-- | Retrieve the tags within the name.
parseNameTags :: T.Text -> ([T.Text], [T.Text])
parseNameTags name =
	partition (\ tag -> not (elem tag releaseQualifiers))
	          (splitSegments (fst (stripGroup name)))
