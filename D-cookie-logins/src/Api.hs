module Api 
    ( RestApiElm
    , RestApi
    , server
    , restApi
    , Person
    , LogEntry
    , Login

    ) where

import Data.Aeson 
import Data.Time (Day, fromGregorian)
import Data.Text (Text, pack)
import Servant
import GHC.Generics
import Servant.API
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Monad.Trans (liftIO)
import Network.Wai.Application.Static
import WaiAppStatic.Types (unsafeToPiece)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

-- two new imports
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()

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

type UnprotectedApiElm = "login" :> ReqBody '[JSON] Login
                                 :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                    , Header "Set-Cookie" SetCookie]
                                                                    NoContent)
                    :<|> "persons" :> Get '[JSON] [Person]

type ProtectedApi = "person" :> Get '[JSON] Person
               :<|> "logs" :> Get '[JSON] [LogEntry]

type RestApiElm auths = (Auth auths Person :> ProtectedApi) :<|> UnprotectedApiElm
type RestApi auths = RestApiElm auths :<|> Raw

-- We'll make servers for each individual section of the api.  
-- this one is much like what we've seen before.

protectedServer :: Connection 
                -> AuthResult Person
                -> Server ProtectedApi
protectedServer conn (Authenticated person) = getPerson 
                                         :<|> getLogs
    where getPerson :: Handler Person
          getPerson = liftIO $ fromDbPerson conn $ personName person

          getLogs :: Handler [LogEntry]
          getLogs = liftIO $ fromDbLogsBy conn $ personName person

protectedServer _ _ = throwAll err401

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
        Nothing -> throwError err401
        Just person -> do
            mApplyCookies <- liftIO $ acceptLogin cs jwts person
            case mApplyCookies of
                Just applyCookies -> do
                    return $ applyCookies NoContent
                _ -> throwError err401

-- unlike the simpler server def, now we have to pull in cookie and jwt settings.

unprotectedServer :: CookieSettings 
                  -> JWTSettings 
                  -> Connection
                  -> Server UnprotectedApiElm 
unprotectedServer cs jwst conn = loginHandler
                            :<|> getPersons
    where loginHandler = checkLogin cs jwst conn 

          getPersons :: Handler [Person]
          getPersons = liftIO $ fromDbAllPersons conn 

staticFiles :: [(FilePath, ByteString)]
staticFiles = [("main.html", $(embedFile "elm/main.html"))]

staticHandler = serveDirectoryWith $
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

restApi = Proxy :: Proxy (RestApi '[Cookie])
