module Api 
    ( RestApiElm
    , RestApi
    , server
    , restApi
    , Person
    , LogEntry
    , Login
    , deleteSessionCookie

    ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Time (Day, UTCTime(UTCTime), fromGregorian)
import Data.Text (Text, pack)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Control.Monad.Trans (liftIO)
import GHC.Generics (Generic)

import qualified Servant
import Servant ((:>), (:<|>)(..), Server, Handler)
import Servant.API (Get, PostNoContent, Capture, JSON, NoContent(..),
                    Header, Headers)

import Database.PostgreSQL.Simple (Connection, Query, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Network.Wai.Application.Static
import WaiAppStatic.Types (unsafeToPiece)

-- two new imports
import Servant.Auth.Server (ToJWT, FromJWT, JWTSettings, CookieSettings,
                            SetCookie)
import qualified Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import qualified Web.Cookie

data Login = Login { username :: Text
                   , password :: Text} deriving Generic
instance FromJSON Login

data Person = Person {
      personName :: String
    , personEmail :: String
    , personDob :: Day
    , personPassword :: String -- in real life these should be kept somewhere safe and salted
} deriving (Show, Generic)
instance ToJSON Person
instance FromJSON Person -- we need this to do a FromJWT
instance ToJWT Person
instance FromJWT Person
instance FromRow Person where
    fromRow = Person <$> field 
                     <*> field
                     <*> field
                     <*> field

personQuery :: Query
personQuery = "select name, email, dob, password \
             \ from Person \
             \ where name = ?"

fromDbPerson :: Connection -> String -> IO Person
fromDbPerson conn name = do
    persons <- query conn personQuery [ name ]  
    return $ head persons

fromDbAllPersons :: Connection -> IO [Person]
fromDbAllPersons conn = do
    persons <- query_ conn "select name, email, dob, password from Person"
    return persons

data LogEntry = LogEntry {
      logAuthor :: String
    , logContent :: Text
    , logDayWritten :: Day
} deriving Generic
instance ToJSON LogEntry
instance FromRow LogEntry where
    fromRow = LogEntry <$> field 
                       <*> field
                       <*> field

fromDbLogsBy :: Connection -> String -> IO [LogEntry]
fromDbLogsBy conn author = do
    logs <- query conn 
                  "select author, content, dayWritten \ 
                 \ from LogEntry \
                 \ where author = ?"
                   [author]
    return logs

-- Our API type gets fragemented into unprotected and protected, elm and non-elm sections.
-- The new thing here is the Login point.  It returns two headers for session cookies and 
-- no content.

type UnprotectedApiElm = "login" :> Servant.ReqBody '[JSON] Login
                                 :> PostNoContent '[JSON]
                                     (Headers '[ Header "Set-Cookie" SetCookie
                                               , Header "Set-Cookie" SetCookie]
                                       NoContent)
                    :<|> "logout" :> PostNoContent '[JSON]
                                     (Headers '[ Header "Set-Cookie" SetCookie ]
                                       NoContent)
                    :<|> "persons" :> Get '[JSON] [Person]

type ProtectedApi = "person" :> Get '[JSON] Person
               :<|> "logs" :> Get '[JSON] [LogEntry]

type RestApiElm auths = (Auth.Auth auths Person :> ProtectedApi) :<|> UnprotectedApiElm
type RestApi auths = RestApiElm auths :<|> Servant.Raw

-- We'll make servers for each individual section of the api.  
-- this one is much like what we've seen before.

protectedServer :: Connection 
                -> Auth.AuthResult Person
                -> Server ProtectedApi
protectedServer conn (Auth.Authenticated person) = getPerson
                                              :<|> getLogs
    where getPerson :: Handler Person
          getPerson = liftIO $ fromDbPerson conn $ personName person

          getLogs :: Handler [LogEntry]
          getLogs = liftIO $ fromDbLogsBy conn $ personName person

protectedServer _ _ = Auth.throwAll Servant.err401

-- things get exciting here.  I have a monad checking for a login in the db
-- and possibly returning back the right Person.  

loginQuery = "select name, email, dob, password \
            \ from Person \
            \ where name = ? and password = ?"

getLoginFromDb :: Connection -> Login -> IO (Maybe Person)
getLoginFromDb conn (Login username password) = do
    possiblePerson <- query conn loginQuery (username , password )
    case possiblePerson of
        [person] -> return $ Just person
        [] -> return Nothing
        _ -> return Nothing

logout :: CookieSettings
       -> Handler (Headers '[Header "Set-Cookie" SetCookie]
                    NoContent)
logout cs = do
  applyCookies <- liftIO $ deleteSessionCookie cs
  return $ applyCookies NoContent


-- this monad is serving the login requests.
-- it's using acceptLogin from servant-auth. 

checkLogin :: CookieSettings
           -> JWTSettings
           -> Connection
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie]
                               NoContent)
checkLogin cs jwts conn login = do
    possiblePerson <- liftIO $ getLoginFromDb conn login
    case possiblePerson of
        Nothing -> Servant.throwError Servant.err401
        Just person -> do
            mApplyCookies <- liftIO $ Auth.acceptLogin cs jwts person
            case mApplyCookies of
                Just applyCookies -> do
                    return $ applyCookies NoContent
                _ -> Servant.throwError Servant.err401

-- unlike the simpler server def, now we have to pull in cookie and jwt settings.

unprotectedServer :: CookieSettings 
                  -> JWTSettings 
                  -> Connection
                  -> Server UnprotectedApiElm 
unprotectedServer cs jwts conn = loginHandler
                            :<|> logoutHandler
                            :<|> getPersons
    where loginHandler = checkLogin cs jwts conn
          logoutHandler = logout cs

          getPersons :: Handler [Person]
          getPersons = liftIO $ fromDbAllPersons conn 

staticFiles :: [(FilePath, ByteString)]
staticFiles = [("main.html", $(embedFile "elm/main.html"))]

staticHandler = Servant.serveDirectoryWith $
  (defaultWebAppSettings $ error "unused") 
    { ssLookupFile = ssLookupFile $ embeddedSettings staticFiles
    ,  ssIndices = map unsafeToPiece ["main.html"]}
          


server :: CookieSettings 
       -> JWTSettings
       -> Connection
       -> Server (RestApi auths)
server cs jwts conn = (     protectedServer conn 
                       :<|> unprotectedServer cs jwts conn)
                      :<|> staticHandler

restApi = Servant.Proxy :: Servant.Proxy (RestApi '[Auth.Cookie])


-- Functions that seem like they should be provided by Servant.Auth, but aren't.
-- I'm not sure whether deleteSessionCookie should also set a CSRF cookie.

expiredSessionCookie :: CookieSettings -> SetCookie
expiredSessionCookie cs = Web.Cookie.def
  { Web.Cookie.setCookieName = Auth.sessionCookieName cs
  , Web.Cookie.setCookieValue = ""
  , Web.Cookie.setCookieExpires = Just $ UTCTime (fromGregorian 2000 1 1) 0
  , Web.Cookie.setCookiePath = Auth.cookiePath cs
  }

deleteSessionCookie :: Servant.AddHeader "Set-Cookie" SetCookie
                                         response withCookie
                    => CookieSettings -> IO (response -> withCookie)
deleteSessionCookie cs = return $ Servant.addHeader $ expiredSessionCookie cs
