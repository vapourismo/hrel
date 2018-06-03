{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (mapM_)

import Control.Applicative
import Control.Monad.Except         hiding (mapM_)
import Control.Monad.Trans.Resource

import Data.ByteString   (ByteString)
import Data.Conduit
import Data.Conduit.List

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Data.Traversal
import HRel.Data.XML
import HRel.Data.XML.Parser (XmlError, XmlMessage, subscribeToXml)
import HRel.Network

withMonadError
    :: Monad m
    => (e -> e')
    -> ConduitT i o (ExceptT e m) a
    -> ConduitT i o (ExceptT e' m) a
withMonadError transformError =
    transPipe (withExceptT transformError)

exampleRequest :: Request
exampleRequest =
    parseRequest_
        "https://thepiratebay.org/rss/top100/200"

data Error
    = RequestError RequestError
    | XmlError XmlError
    | TraversalError
    deriving Show

exampleTraversal :: Monad m => XmlTraversal (ConduitT XmlMessage ByteString m) ()
exampleTraversal =
    void $ child "rss" $ child "channel" $ many $ child "item" $ do
        title <- child "title" text
        lift (yield title)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    result <- runResourceT $ runExceptT $ runConduit $
        withMonadError RequestError (requestConduit manager exampleRequest)
        .| withMonadError XmlError subscribeToXml
        .| (traverseConduit exampleTraversal >>= maybe (throwError TraversalError) pure)
        .| mapM_ (liftIO . print)

    case result of
        Left error -> print error
        Right _    -> pure ()
