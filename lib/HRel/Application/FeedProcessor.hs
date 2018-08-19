{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module HRel.Application.FeedProcessor
    ( Input
    , inputInfo
    , main
    )
where

import Options.Applicative

data Input = Input

inputInfo :: ParserInfo Input
inputInfo =
    info (pure Input) (progDesc "Feed processor")

main :: Input -> IO ()
main Input =
    pure ()
