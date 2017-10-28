module Api 
    ( RestApi
    , server
    , restApi
    ) where


import Data.Aeson -- encoding for, say, json
import Data.Time (Day, fromGregorian)
import Data.Text (Text, pack)
import Servant
import GHC.Generics
import Servant.API

-- three new imports

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Monad.Trans (liftIO)

data Person = Person {
      personName :: String
    , personEmail :: String
    , personDob :: Day
    , password :: String -- in real life these should be kept somewhere safe and salted
} deriving Generic
instance ToJSON Person

-- For types that relate to something in posgres, we can implement ToRow and FromRow
-- to make the transfer easier.  We have to specify the transformation manually by
-- implementing the fromRow function.
--
-- I don't know what the <$> and <*> things do.
--
-- see https://hackage.haskell.org/package/postgresql-simple-0.5.3.0/docs/Database-PostgreSQL-Simple.html

instance FromRow Person where
    fromRow = Person <$> field 
                     <*> field
                     <*> field
                     <*> field

-- talking to a database is a stateful effect.  We have to do it in an IO monad.
-- These are some helper functions.
-- later on we'll use liftIO to get this inside the Handler monand.

personQuery :: Query
personQuery = "select name, email, dob, password \
             \ from Person \
             \ where name = ?"

personFromDb :: Connection -> String -> IO Person
personFromDb conn name = do
    persons <- query conn personQuery [ name ]  
    -- for the life of me I can't figure out why 
    -- `[Only person] <- query conn ...`
    -- doesn't work.  Maybe my earlier examples were using an extension I didn't catch 
    -- here.  oh well.
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

type RestApi = "person" :> Capture "name" String :> Get '[JSON] Person
          :<|> "persons" :> Get '[JSON] [Person]
          :<|> "logs" :> Capture "author" String :> Get '[JSON] [LogEntry]  

-- we want our DB connection to be server wide.  So we add it in here.
-- we'll let app/Main.hs create the connection.
-- in real life you'd probably want some sort of connection pool.

server :: Connection -> Server RestApi
server conn = getPerson 
         :<|> getAllPersons
         :<|> getLogs 
    where getPerson :: String -> Handler Person
-- we're using liftIO to get bring the monad into the Handler one the Server is using.
          getPerson name = liftIO $ personFromDb conn name

          getAllPersons :: Handler [Person]
          getAllPersons = liftIO $ fromDbAllPersons conn

          getLogs :: String -> Handler [LogEntry]
          getLogs author = liftIO $ fromDbLogsBy conn author

restApi :: Proxy RestApi
restApi = Proxy


