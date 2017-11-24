module Api 
    ( RestApi
    , server1
    , server2
    , server3
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

-- The most direct change is: instead of taking a Connection, we take a pool and
-- grab a connection for it in each handler.
server1 :: Pool Connection -> Servant.Server RestApi
server1 pool = getPerson
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

-- We can factor out `withResource pool`.
server2 :: Pool Connection -> Servant.Server RestApi
server2 pool = getPerson
          :<|> getAllPersons
          :<|> getLogs
    where getPerson :: String -> Handler Person
          getPerson name = withPool $ \conn ->
            liftIO $ personFromDb conn name

          getAllPersons :: Handler [Person]
          getAllPersons = withPool $ \conn ->
            liftIO $ fromDbAllPersons conn

          getLogs :: String -> Handler [LogEntry]
          getLogs author = withPool $ \conn ->
            liftIO $ fromDbLogsBy conn author

          withPool :: (Connection -> Handler a) -> Handler a
          withPool = withResource pool

-- We can also move `withPool` into the argument instead of the `where`. This
-- isn't a big gain here, but if you have nested servers (as in
-- e.g. D-cookie-logins) you don't need to implement `withPool` in all of them.
--
-- I don't particularly understand this. I think that without the `forall` the
-- compiler thinks `a` is a single specific type. It doesn't like that `a` gets
-- specified as `Person` in `getPerson`, and also doesn't like that the other
-- handlers specify `a` as something else.
server3 :: (forall a. (Connection -> Handler a) -> Handler a)
        -> Servant.Server RestApi
server3 withPool = getPerson
              :<|> getAllPersons
              :<|> getLogs
    where getPerson :: String -> Handler Person
          getPerson name = withPool $ \conn ->
            liftIO $ personFromDb conn name

          getAllPersons :: Handler [Person]
          getAllPersons = withPool $ \conn ->
            liftIO $ fromDbAllPersons conn

          getLogs :: String -> Handler [LogEntry]
          getLogs author = withPool $ \conn ->
            liftIO $ fromDbLogsBy conn author

restApi :: Servant.Proxy RestApi
restApi = Servant.Proxy
