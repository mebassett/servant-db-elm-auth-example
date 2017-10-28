module Main where

import Api
import Servant
import Network.Wai
import Network.Wai.Handler.Warp


main :: IO ()
main = do
    run 8081 app
