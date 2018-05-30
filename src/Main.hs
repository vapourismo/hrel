module Main (main) where

import Control.Monad.Except

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import HRel.Network

exampleRequest :: Request
exampleRequest =
    parseRequest_
        "https://www.xrel.to/releases-usrss.html?u=20470&s=ee663473a8da8a161902c908326ebe1c&favs=1"

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    result <- runExceptT (requestResponseBody manager exampleRequest)
    case result of
        Left error -> print error
        Right result -> print result
