module Api 
    ( RestApi
    , server
    , restApi
    ) where


import Data.Aeson (ToJSON) -- encoding for, say, json
import Data.Time (Day, fromGregorian)
import Data.Text (Text, pack)
import qualified Servant
import Servant (Handler)
import GHC.Generics (Generic)
import Servant.API ((:>), (:<|>)(..), Get, Capture, JSON)

-- three new imports

import Database.PostgreSQL.Simple (Connection, Query, query, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Control.Monad.Trans (liftIO)

data Person = Person {
      personName :: String
    , personEmail :: String
    , personDob :: Day
    , password :: String -- in real life these should be kept somewhere safe and salted
} deriving Generic
instance ToJSON Person

-- For types that relate to something in posgres, we can implement ToRow and
-- FromRow to make the transfer easier.
--
-- <$> is a synonym for fmap: `a <$> b` is the same as
--     (b >>= return . a)
-- or
--     do b' <- b
--        return $ a b'
--
-- <*> is sequential application: `a <*> b` is the same as
--     a >>= (\a' -> (b >>= return . a'))
-- or
--     do a' <- a
--        b' <- b
--        return $ a' b'
--
-- Then `a <$> b <*> c` (more explicitly, `(a <$> b) <*> c`) is the same as
--     do ab' <- do b' <- b
--                  return $ a b'
--        c' <- c
--        return $ ab' c'
-- which simplifies to
--     do b' <- b
--        c' <- c
--        return $ a b' c'
--
-- And `a <$> b <*> c <*> d` is the same as
--     do abc' <- do b' <- b
--                   c' <- c
--                   return $ a b' c'
--        d' <- d
--        return $ abc' d'
-- which simplifies to
--     do b' <- b
--        c' <- c
--        d' <- d
--        return $ a b' c' d'
--
-- In this case, we don't actually need to implement `fromRow`, since `FromRow`
-- has a generic implementation. So simply
--     instance FromRow Person
-- would have the same effect.
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
-- For info on these, see:
-- https://hackage.haskell.org/package/postgresql-simple-0.5.3.0/docs/Database-PostgreSQL-Simple.html

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

server :: Connection -> Servant.Server RestApi
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

restApi :: Servant.Proxy RestApi
restApi = Servant.Proxy
