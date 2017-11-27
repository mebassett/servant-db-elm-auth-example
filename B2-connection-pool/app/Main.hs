module Main where

import Api
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool, withResource)
import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Simple as DB

connectAndPrint :: B.ByteString -> IO Connection
connectAndPrint connStr = do putStrLn "making a new connection"
                             DB.connectPostgreSQL connStr

main :: IO ()
main = do
  dbName <- fromMaybe "haskell-stuff" <$> lookupEnv "SERVANT_EXAMPLE_DB"
  let connStr = B.pack $ "dbname='" ++ dbName ++ "'"

  -- createPool takes five args:
  -- 1. IO Connection: function to create a connection
  -- 2. Connection -> IO (): function to close a connection
  -- 3. Int: number of stripes
  -- 4. NominalDiffTime: time a connection can be idle before closing it
  -- 5. Int: max number of open connections per stripe
  -- See https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html
  pool <- createPool (connectAndPrint connStr) DB.close 1 10 10

  -- run 8081 $ serve restApi $ server1 pool
  -- run 8081 $ serve restApi $ server2 pool
  -- run 8081 $ serve restApi $ server3 (withResource pool)
  run 8081 $ serve restApi $ server4 pool
