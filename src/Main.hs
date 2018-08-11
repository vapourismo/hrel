{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative

import qualified HRel.Application.FeedProcessor as FeedProcessor
import qualified HRel.Application.Hub           as Hub
import qualified HRel.Application.Test          as Test

data Input
    = FeedProcessor FeedProcessor.Input
    | Hub Hub.Input
    | Test

inputInfo :: ParserInfo Input
inputInfo =
    info (helper <*> commandParser) fullDesc
    where
        commandParser = foldl (\ other cmd -> hsubparser cmd <|> other) empty
            [ command "feed-processor" (FeedProcessor <$> FeedProcessor.inputInfo)
            , command "hub"            (Hub           <$> Hub.inputInfo)
            , command "test"           (info (pure Test) mempty)
            ]

main :: IO ()
main =
    execParser inputInfo >>= \case
        FeedProcessor info -> FeedProcessor.main info
        Hub info           -> Hub.main info
        Test               -> Test.main
