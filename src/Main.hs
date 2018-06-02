{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (mapM_)

import Control.Monad.Except         hiding (mapM_)
import Control.Monad.Trans.Resource

import Data.Conduit
import Data.Conduit.List

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Data.XML.Parser (XmlError, subscribeToXml)
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
    deriving Show

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    result <- runResourceT $ runExceptT $ runConduit $
        withMonadError RequestError (requestConduit manager exampleRequest)
        .| withMonadError XmlError subscribeToXml
        .| mapM_ (liftIO . print)

    case result of
        Left error     -> print error
        Right messages -> print messages
