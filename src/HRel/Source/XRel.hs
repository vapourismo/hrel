{-# LANGUAGE OverloadedStrings #-}

module HRel.Source.XRel (
	xrelFavourites
) where

import HRel.Source
import HRel.Markup

-- | Fetch results from Atom feed.
fetchFromAtom :: String -> Manager -> IO [Release]
fetchFromAtom url mgr = do
	req <- parseUrl url
	withTextResponse req mgr (pure . maybe [] id . runNodeFilter rssFilter . fromMarkup')
	where
		rssFilter =
			relativeTag "feed" $
				foreachTag "entry" $
					forTag "title" (fmap makeRelease text)

-- | Favourites on 'xrel.to'
xrelFavourites :: Word -> String -> Word -> Aggregator Release
xrelFavourites user session list =
	Aggregator (fetchFromAtom ("http://www.xrel.to/releases-usrss.html?u=" ++ show user
	                           ++ "&s=" ++ session
	                           ++ "&favs=" ++ show list))
