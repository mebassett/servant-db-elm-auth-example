module Main where

import Api
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple (connectPostgreSQL)

-- one new import.
import Network.Wai.Middleware.Cors (simpleCors)
main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname='haskell-stuff'" -- Params should be ENV VAR

    -- the simpleCors middleware injects an Access-Control-Allow-Origin header
    -- this lets us server the rest api to different domains. Any domain, in fact.
    -- NOT a good idea in real life.
    -- We're using it here just to make debugging simple.  Run elm reactor on port 8000
    -- and this server on 8081
    run 8081 $ simpleCors $ serve restApi $ server conn
