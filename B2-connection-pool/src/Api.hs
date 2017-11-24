module Api 
    ( RestApi
    , server1
    , restApi
    ) where


import Data.Aeson (ToJSON)
import Data.Time (Day, fromGregorian)
import Data.Text (Text, pack)
import Data.Pool (Pool, withResource)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple (Connection, Query, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Control.Monad.Trans (liftIO)

import qualified Servant
import Servant (Handler)
import Servant.API ((:>), (:<|>)(..), Get, Capture, JSON)

-- Most of this is the same as in B-postgres-data. Only the definition of
-- `server` needs to change.

data Person = Person {
      personName :: String
    , personEmail :: String
    , personDob :: Day
    , password :: String
} deriving Generic
instance ToJSON Person
instance FromRow Person

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
instance FromRow LogEntry

fromDbLogsBy :: Connection -> String -> IO [LogEntry]
fromDbLogsBy conn author = do
    logs <- query conn 
                  "select author, content, dayWritten \ 
                 \ from LogEntry \
                 \ where author = ?"
                   [author]
    return logs

type RestApi = "person" :> Capture "name" String :> Get '[JSON] Person
          :<|> "persons" :> Get '[JSON] [Person]
          :<|> "logs" :> Capture "author" String :> Get '[JSON] [LogEntry]  

-- The direct option is, instead of taking a Connection, we take a pool and grab
-- a connection for it in each handler.

server :: Pool Connection -> Servant.Server RestApi
server pool = getPerson
         :<|> getAllPersons
         :<|> getLogs
    where getPerson :: String -> Handler Person
          getPerson name = withResource pool $ \conn ->
            liftIO $ personFromDb conn name

          getAllPersons :: Handler [Person]
          getAllPersons = withResource pool $ \conn ->
            liftIO $ fromDbAllPersons conn

          getLogs :: String -> Handler [LogEntry]
          getLogs author = withResource pool $ \conn ->
            liftIO $ fromDbLogsBy conn author

restApi :: Servant.Proxy RestApi
restApi = Servant.Proxy
