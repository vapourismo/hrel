{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (mapM_)

import Control.Monad.Except hiding (mapM_)

import Data.Aeson
import Data.Attoparsec.ByteString
import Data.ByteString            (ByteString)
import Data.Conduit
import Data.Conduit.Combinators   (stdin)
import Data.Conduit.List          (mapM_)

data ParseError =
    ParseError [String] String
    deriving (Show, Eq)

jsonConduit :: MonadError ParseError m => ConduitT ByteString Value m ()
jsonConduit =
    loop (Partial (parse json))
    where
        loop = \case
            Done leftOver result -> do
                yield result
                loop (parse json leftOver)

            Fail _ contexts message ->
                throwError (ParseError contexts message)

            Partial continue ->
                await >>= \case
                    Nothing -> pure ()
                    Just input -> loop (continue input)

data DumpedTorrent =
    DumpedTorrent
        { dtTitle  :: String
        , dtMagnet :: String }
    deriving (Show, Eq)

instance FromJSON DumpedTorrent where
    parseJSON (Object hashMap) =
        DumpedTorrent
            <$> hashMap .: "Title"
            <*> hashMap .: "Magnet"

    parseJSON _ = mzero

withValue :: MonadIO m => Value -> m ()
withValue value = liftIO $ do
    case fromJSON value of
        Error message -> putStrLn message
        Success value -> print (value :: DumpedTorrent)

main :: IO ()
main = do
    result <- runExceptT (runConduit (stdin .| jsonConduit .| mapM_ withValue))
    print result
