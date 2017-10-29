module Main where

import Api
import Servant
import Servant.Auth.Server
import qualified Data.Time as Time
import Servant.Auth.Server.Internal.ConfigTypes (SameSite (AnySite))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Network.Wai.Middleware.Cors (simpleCors)

main :: IO ()
main = do
    myKey <- generateKey
    now <- Time.getCurrentTime
    conn <- connectPostgreSQL "dbname='haskell-stuff'" -- Params should be ENV VAR

    let jwtCfg = defaultJWTSettings myKey
        cookieSettings =
          defaultCookieSettings
           { cookiePath = Just "/",
             cookieExpires = Just now { Time.utctDay = Time.addDays 30 (Time.utctDay now)},
             xsrfCookieName = "XSRF-TOKEN",
             xsrfHeaderName = "X_XSRF_TOKEN",--servant-elm has troulbe with - in names.
             cookieIsSecure = Servant.Auth.Server.NotSecure,
             cookieSameSite = AnySite
           }
        cfg = cookieSettings :. jwtCfg :. EmptyContext

    withStdoutLogger $ \appLogger -> do
        let settings = setPort 8081 $ setLogger appLogger defaultSettings
        runSettings settings $ simpleCors $ serveWithContext restApi cfg $ server cookieSettings jwtCfg conn
