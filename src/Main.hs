{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative

import qualified HRel.Application.FeedProcessor as FeedProcessor

newtype Input = FeedProcessor FeedProcessor.Input

inputInfo :: ParserInfo Input
inputInfo =
    info (helper <*> commandParser) fullDesc
    where
        commandParser =
            FeedProcessor <$> hsubparser (command "feed-processor" FeedProcessor.inputInfo)

main :: IO ()
main =
    execParser inputInfo >>= \case
        FeedProcessor info -> FeedProcessor.main info
