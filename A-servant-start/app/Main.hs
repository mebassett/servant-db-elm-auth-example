module Main where

import Api
import Servant
import Network.Wai
import Network.Wai.Handler.Warp


-- this creates an abstract runnable web application for the server and api we described.

app :: Application
app = serve restApi server

main :: IO ()
main = do
    run 8081 app
