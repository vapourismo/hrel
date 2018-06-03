{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (mapM_)

import Control.Applicative
import Control.Monad.Except         hiding (mapM_)
import Control.Monad.Trans.Resource

import Data.ByteString   (ByteString)
import Data.Conduit
import Data.Conduit.List (mapM_)
import Data.Foldable     (for_)

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Data.Traversal
import HRel.Data.XML
import HRel.Data.XML.Parser (XmlError, XmlMessage, subscribeToXml)
import HRel.Network

withMonadError
    :: (Monad m, MonadError e' m)
    => (e -> e')
    -> ConduitT i o (ExceptT e m) a
    -> ConduitT i o m a
withMonadError transformError =
    transPipe (runExceptT >=> either (throwError . transformError) pure)

exampleRequests :: [Request]
exampleRequests =
    map parseRequest_
        [ "https://thepiratebay.org/rss/top100/0"
        , "https://thepiratebay.org/rss/top100/100"
        , "https://thepiratebay.org/rss/top100/101"
        , "https://thepiratebay.org/rss/new/101"
        , "https://thepiratebay.org/rss/top100/102"
        , "https://thepiratebay.org/rss/new/102"
        , "https://thepiratebay.org/rss/top100/103"
        , "https://thepiratebay.org/rss/new/103"
        , "https://thepiratebay.org/rss/top100/104"
        , "https://thepiratebay.org/rss/new/104"
        , "https://thepiratebay.org/rss/top100/199"
        , "https://thepiratebay.org/rss/new/199"
        , "https://thepiratebay.org/rss/top100/200"
        , "https://thepiratebay.org/rss/top100/201"
        , "https://thepiratebay.org/rss/new/201"
        , "https://thepiratebay.org/rss/top100/202"
        , "https://thepiratebay.org/rss/new/202"
        , "https://thepiratebay.org/rss/top100/203"
        , "https://thepiratebay.org/rss/new/203"
        , "https://thepiratebay.org/rss/top100/204"
        , "https://thepiratebay.org/rss/new/204"
        , "https://thepiratebay.org/rss/top100/205"
        , "https://thepiratebay.org/rss/new/205"
        , "https://thepiratebay.org/rss/top100/206"
        , "https://thepiratebay.org/rss/new/206"
        , "https://thepiratebay.org/rss/top100/207"
        , "https://thepiratebay.org/rss/new/207"
        , "https://thepiratebay.org/rss/top100/208"
        , "https://thepiratebay.org/rss/new/208"
        , "https://thepiratebay.org/rss/top100/209"
        , "https://thepiratebay.org/rss/new/209"
        , "https://thepiratebay.org/rss/top100/299"
        , "https://thepiratebay.org/rss/new/299"
        , "https://thepiratebay.org/rss/top100/300"
        , "https://thepiratebay.org/rss/top100/301"
        , "https://thepiratebay.org/rss/new/301"
        , "https://thepiratebay.org/rss/top100/302"
        , "https://thepiratebay.org/rss/new/302"
        , "https://thepiratebay.org/rss/top100/303"
        , "https://thepiratebay.org/rss/new/303"
        , "https://thepiratebay.org/rss/top100/304"
        , "https://thepiratebay.org/rss/new/304"
        , "https://thepiratebay.org/rss/top100/305"
        , "https://thepiratebay.org/rss/new/305"
        , "https://thepiratebay.org/rss/top100/306"
        , "https://thepiratebay.org/rss/new/306"
        , "https://thepiratebay.org/rss/top100/399"
        , "https://thepiratebay.org/rss/new/399"
        , "https://thepiratebay.org/rss/top100/400"
        , "https://thepiratebay.org/rss/top100/401"
        , "https://thepiratebay.org/rss/new/401"
        , "https://thepiratebay.org/rss/top100/402"
        , "https://thepiratebay.org/rss/new/402"
        , "https://thepiratebay.org/rss/top100/403"
        , "https://thepiratebay.org/rss/new/403"
        , "https://thepiratebay.org/rss/top100/404"
        , "https://thepiratebay.org/rss/new/404"
        , "https://thepiratebay.org/rss/top100/405"
        , "https://thepiratebay.org/rss/new/405"
        , "https://thepiratebay.org/rss/top100/406"
        , "https://thepiratebay.org/rss/new/406"
        , "https://thepiratebay.org/rss/top100/407"
        , "https://thepiratebay.org/rss/new/407"
        , "https://thepiratebay.org/rss/top100/408"
        , "https://thepiratebay.org/rss/new/408"
        , "https://thepiratebay.org/rss/top100/499"
        , "https://thepiratebay.org/rss/new/499"
        , "https://thepiratebay.org/rss/top100/500"
        , "https://thepiratebay.org/rss/top100/501"
        , "https://thepiratebay.org/rss/new/501"
        , "https://thepiratebay.org/rss/top100/502"
        , "https://thepiratebay.org/rss/new/502"
        , "https://thepiratebay.org/rss/top100/503"
        , "https://thepiratebay.org/rss/new/503"
        , "https://thepiratebay.org/rss/top100/504"
        , "https://thepiratebay.org/rss/new/504"
        , "https://thepiratebay.org/rss/top100/505"
        , "https://thepiratebay.org/rss/new/505"
        , "https://thepiratebay.org/rss/top100/506"
        , "https://thepiratebay.org/rss/new/506"
        , "https://thepiratebay.org/rss/top100/599"
        , "https://thepiratebay.org/rss/new/599"
        , "https://thepiratebay.org/rss/top100/600"
        , "https://thepiratebay.org/rss/top100/601"
        , "https://thepiratebay.org/rss/new/601"
        , "https://thepiratebay.org/rss/top100/602"
        , "https://thepiratebay.org/rss/new/602"
        , "https://thepiratebay.org/rss/top100/603"
        , "https://thepiratebay.org/rss/new/603"
        , "https://thepiratebay.org/rss/top100/604"
        , "https://thepiratebay.org/rss/new/604"
        , "https://thepiratebay.org/rss/top100/605"
        , "https://thepiratebay.org/rss/new/605"
        , "https://thepiratebay.org/rss/top100/699"
        , "https://thepiratebay.org/rss/new/699"
        , "https://thepiratebay.org/rss/top100/0" ]

makeRequest
    :: ( MonadResource m
       , MonadError Error m )
    => Manager
    -> Request
    -> ConduitT ByteString o m ()
    -> ConduitT i o m ()
makeRequest manager request handler =
    withMonadError RequestError (requestConduit manager request)
    .| handler

handleXml
    :: MonadError Error m
    => XmlTraversal (ConduitT XmlMessage o m) ()
    -> ConduitT ByteString o m ()
handleXml traversal =
    withMonadError XmlError subscribeToXml
    .| (traverseConduit traversal >>= maybe (throwError TraversalError) pure)

data Error
    = RequestError RequestError
    | XmlError XmlError
    | TraversalError
    deriving Show

tpbTraversal :: Monad m => XmlTraversal (ConduitT XmlMessage (ByteString, ByteString) m) ()
tpbTraversal =
    void $ child "rss" $ child "channel" $ many $ child "item" $ do
        title <- child "title" text
        magnetUri <- child "torrent" (child "magnetURI" text)
        lift (yield (title, magnetUri))

withRequest
    :: ( MonadResource m
       , MonadError Error m )
    => Manager
    -> Request
    -> ConduitT i (ByteString, ByteString) m ()
withRequest manager request =
    catchError (makeRequest manager request (handleXml tpbTraversal)) (liftIO . print)

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    result <- runResourceT $ runExceptT $ runConduit $
        for_ exampleRequests (withRequest manager)
        .| mapM_ (liftIO . print)

    case result of
        Left error -> print error
        Right _    -> pure ()
