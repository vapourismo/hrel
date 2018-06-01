{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Control.Monad.Except

import qualified Data.ByteString as ByteString

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Xeno.Types

import HRel.Network
import HRel.XML

withMonadError
    :: MonadError e' m
    => (e -> e')
    -> ExceptT e m a
    -> m a
withMonadError transformError action =
    runExceptT (withExceptT transformError action) >>= liftEither

exampleRequest :: Request
exampleRequest =
    parseRequest_
        "https://www.xrel.to/releases-usrss.html?u=20470&s=ee663473a8da8a161902c908326ebe1c&favs=1"

exampleTraversal :: XmlTraversal [(ByteString.ByteString, ByteString.ByteString)]
exampleTraversal =
    child "feed" $
        children "entry" $
            (,) <$> child "title" text
                <*> child "link" (attribute "href")

data Error
    = RequestError RequestError
    | XmlError XenoException
    deriving Show

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    result <- runExceptT $ do
        contents <- withMonadError RequestError (requestResponseBody manager exampleRequest)
        withMonadError XmlError (liftEither (runXmlTraversal exampleTraversal contents))

    case result of
        Left error   -> print error
        Right result -> print result
