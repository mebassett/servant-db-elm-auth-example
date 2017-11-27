module Api 
    ( RestApi
    , server1
    , server2
    , server3
    , server4
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
import qualified Control.Monad.Reader as R

import qualified Servant
import qualified Servant.Server
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
          getPerson name = withConn $ \conn ->
            liftIO $ personFromDb conn name

          getAllPersons :: Handler [Person]
          getAllPersons = withConn $ \conn ->
            liftIO $ fromDbAllPersons conn

          getLogs :: String -> Handler [LogEntry]
          getLogs author = withConn $ \conn ->
            liftIO $ fromDbLogsBy conn author

          withConn :: (Connection -> Handler a) -> Handler a
          withConn = withResource pool

-- We can also move `withConn` into the argument instead of the `where`. This
-- isn't a big gain here, but if you have nested servers (as in e.g.
-- D-cookie-logins) you don't need to implement `withPool` in all of them.
--
-- I don't particularly understand this. I think that without the `forall` the
-- compiler thinks `a` is a single specific type. It doesn't like that `a` gets
-- specified as `Person` in `getPerson`, and also doesn't like that the other
-- handlers specify `a` as something else.
--
-- Requires language extension `Rank2Types` or (more powerful) `RankNTypes`, to
-- allow `forall` in this context.
server3 :: (forall a. (Connection -> Handler a) -> Handler a)
        -> Servant.Server RestApi
server3 withConn = getPerson
              :<|> getAllPersons
              :<|> getLogs
    where getPerson :: String -> Handler Person
          getPerson name = withConn $ \conn ->
            liftIO $ personFromDb conn name

          getAllPersons :: Handler [Person]
          getAllPersons = withConn $ \conn ->
            liftIO $ fromDbAllPersons conn

          getLogs :: String -> Handler [LogEntry]
          getLogs author = withConn $ \conn ->
            liftIO $ fromDbLogsBy conn author


-- Finally we can provide `withConn` from a monad of our own creation. This adds
-- a lot of complexity outside the server, and doesn't save much within. But the
-- general pattern is powerful, and could be used to add other monady things at
-- the same time.
--
-- Requires language extension `GeneralizedNewtypeDeriving`, and Servant 0.12.
class MonadDB m where
  withConn :: (Connection -> m a) -> m a

newtype H a = H { runH :: R.ReaderT (Pool Connection) Handler a }
  deriving (Functor, Applicative, Monad, R.MonadIO)

instance MonadDB H where
  withConn f = H $ do
    pool <- R.ask
    withResource pool $ \conn -> runH (f conn)

hToHandler :: Pool Connection -> H a -> Handler a
hToHandler p h = R.runReaderT (runH h) p

server4H :: Servant.ServerT RestApi H
server4H = getPerson
      :<|> getAllPersons
      :<|> getLogs
  where getPerson :: String -> H Person
        getPerson name = withConn $ \conn ->
          liftIO $ personFromDb conn name

        getAllPersons :: H [Person]
        getAllPersons = withConn $ \conn ->
          liftIO $ fromDbAllPersons conn

        getLogs :: String -> H [LogEntry]
        getLogs author = withConn $ \conn ->
          liftIO $ fromDbLogsBy conn author

server4 :: Pool Connection -> Servant.Server RestApi
server4 pool = Servant.hoistServer restApi (hToHandler pool) server4H

-- Note something that *doesn't* seem to work. You can't have
--     server conn = ...
-- and then call `withResource pool server`. The server isn't of the right
-- type. It's possible someone could get that to work, but I don't know how the
-- pool would behave if so.

restApi :: Servant.Proxy RestApi
restApi = Servant.Proxy
