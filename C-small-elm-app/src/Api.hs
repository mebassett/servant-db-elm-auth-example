module Api 
    ( RestApiElm
    , RestApi
    , server
    , restApi
    , Person
    , LogEntry 

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

-- four new imports
import Network.Wai.Application.Static
import WaiAppStatic.Types (unsafeToPiece)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

data Person = Person {
      personName :: String
    , personEmail :: String
    , personDob :: Day
    , password :: String -- in real life these should be kept somewhere safe and salted
} deriving Generic
instance ToJSON Person
instance FromRow Person where
    fromRow = Person <$> field 
                     <*> field
                     <*> field
                     <*> field

personQuery :: Query
personQuery = "select name, email, dob, password \
             \ from Person \
             \ where name = ?"

personFromDb :: Connection -> String -> IO Person
personFromDb conn name = do
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

-- we've changed our definitions here.  We're adding a "Raw" endpoint.
-- "Raw" is basically an escape hatch from the servant framework.
-- we're using it to serve static html from elm.
-- However, the elm code generator won't know what to do with it.
-- So we are excluding it from the "Elm API". 

type RestApiElm = "person" :> Capture "name" String :> Get '[JSON] Person
          :<|> "persons" :> Get '[JSON] [Person]
          :<|> "logs" :> Capture "author" String :> Get '[JSON] [LogEntry]  

type RestApi = RestApiElm  :<|> Raw
-- creating our files. 
-- note that the static files are compiled into haskell.
--
-- the funny $(embedFile) syntax is enabled to the TemplateHaskell language extension.
staticFiles :: [(FilePath, ByteString)]
staticFiles = [("main.html", $(embedFile "elm/main.html"))]


-- the staticHandler returned from serveDirectoryWith is actually a Server
-- not a Handler Monad.  So I do weird stuff with the parans to get the 
-- compile to take the :<|> combinator.
server :: Connection -> Server RestApi
server conn = (
               getPerson 
          :<|> getAllPersons
          :<|> getLogs 
          )
          :<|> staticHandler
    where getPerson :: String -> Handler Person
          getPerson name = liftIO $ personFromDb conn name

          getAllPersons :: Handler [Person]
          getAllPersons = liftIO $ fromDbAllPersons conn

          getLogs :: String -> Handler [LogEntry]
          getLogs author = liftIO $ fromDbLogsBy conn author
        
          staticHandler :: Tagged Handler Application
          staticHandler = serveDirectoryWith $
              (defaultWebAppSettings $ error "unused") 
                { ssLookupFile = ssLookupFile $ embeddedSettings staticFiles
                ,  ssIndices = map unsafeToPiece ["main.html"]}

restApi :: Proxy RestApi
restApi = Proxy


