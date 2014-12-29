{-# LANGUAGE OverloadedStrings #-}

module HRel.Release where

import Control.Monad
import Control.Applicative

import Data.Word
import Data.Either
import Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy as T


-- | Video resolution
data RelVideoRes
	= Rel720p
	| Rel1080p
	deriving Show

-- |
data ReleaseTag
	= RelSource
	| RelVideoRes RelVideoRes
	| RelAudio
	| RelEpisode Word Word
	| RelSeason Word
	| RelSeasonRange Word Word
	| RelYear Word
	| RelPart Word
	deriving Show

-- | Episode
tagEpisode :: Parser ReleaseTag
tagEpisode =
	RelEpisode <$> fmap read (seasonPref *> some digit)
	           <*> fmap read (episodePref *> some digit)
	where
		seasonPref = (string "season" <* option '.' (char '.')) <|> string "s"
		episodePref = (string "episode" <* option '.' (char '.')) <|> string "e"

-- | Season
tagSeason :: Parser ReleaseTag
tagSeason = seasonPref *> seasonValue
	where
		seasonPref = (string "season" <* option '.' (char '.')) <|> string "s"
		seasonValue = seasonRange <|> fmap (RelSeason . read) (some digit)
		seasonRange =
			RelSeasonRange <$> fmap read (some digit)
			               <*> fmap read (char '-' *> some digit)

-- | Year
tagYear :: Parser ReleaseTag
tagYear = do
	ys <- some digit
	let y = read ys
	guard (y > 1850)
	return (RelYear y)

-- | Video Resolution
tagVideoRes :: Parser ReleaseTag
tagVideoRes =
	(RelVideoRes Rel1080p <$ string "1080p") <|> (RelVideoRes Rel720p <$ string "720p")

-- | Fetch release tags
buildRelTags :: T.Text -> (Maybe T.Text, [ReleaseTag])
buildRelTags name =
	case parseMaybe (sepBy1 segment (choice [char '.', char '-', char '_'])) (T.toLower name) of
		Just tags -> (findTitle [] tags, rights tags)
		Nothing -> (Nothing, [])
	where
		segment = eitherP (fmap T.pack (some letter))
		                  (choice [tagEpisode, tagSeason, tagYear, tagVideoRes])

		findTitle _ [] = Nothing
		findTitle t (Left x : ys) = findTitle (t ++ [x]) ys
		findTitle t (Right x : _) =
			case x of
				RelEpisode _ _ -> Just (fixTitle t)
				RelSeason _ -> Just (fixTitle t)
				RelSeasonRange _ _ -> Just (fixTitle t)
				RelYear _ -> Just (fixTitle t)
				RelPart _ -> Just (fixTitle t)
				_ -> Nothing
			where
				fixTitle zs = T.intercalate " " (map T.toTitle zs)

-- | Release
data Release = Release { relOrgName :: T.Text
                       , relTitle   :: Maybe T.Text
                       , relTags    :: [ReleaseTag]
                       , relAuthor  :: T.Text }
	deriving Show

-- | Run a parser against an input text.
parseMaybe :: Parser a -> T.Text -> Maybe a
parseMaybe p t = maybeResult (parse p t)

-- | Parse a release name
parseRelName :: T.Text -> Maybe Release
parseRelName name =
	if length segments > 1 then
		let
			releaseName = T.concat (init segments)
			(mbTitle, tags) = buildRelTags releaseName
		in Just (Release releaseName mbTitle tags (last segments))
	else
		Nothing
	where
		segments = T.split (== '-') name
