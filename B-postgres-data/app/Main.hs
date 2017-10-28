module Main where

import Api
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple (connectPostgreSQL)

main :: IO ()
main = do
    -- now we create the connection and pass it into the server, before turning it into
    -- a runnable app.
    conn <- connectPostgreSQL "dbname='haskell-stuff'" -- Params should be ENV VAR
    run 8081 $ serve restApi $ server conn
