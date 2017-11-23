module Api 
    ( RestApi
    , server
    , restApi
    ) where


import Data.Aeson (ToJSON) -- encoding for, say, json
import Data.Char (toLower)
import Data.Time (Day, fromGregorian)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import qualified Servant
import Servant (Handler, (:>), (:<|>)(..))
import Servant.API (Get, Capture, JSON)


-- This app serves logs from two people trying to survive after "The Event", 
-- having nothing to do but try to find something to eat and watch The Quiz 
-- Broadcast.
--
-- We'll start by setting up some data structures and some dummy data

data Person = Person {
      personName :: String
    , personEmail :: String
    , personDob :: Day
    , password :: String -- in real life these should be kept somewhere safe and salted
} deriving Generic

-- see that `deriving Generic` up there? This is enabled by our Haskell language
-- extension `DerivingGeneric` in our cabal file.  It makes it super easy to implement
-- our ToJSON interface.  We just call it a ToJSON.

instance ToJSON Person -- now we have an Aeson encoder to JSON.

data LogEntry = LogEntry {
      logAuthor :: Person
    , logContent :: Text
    , logDayWritten :: Day
} deriving Generic

instance ToJSON LogEntry

john :: Person
john = Person { personName = "John"
              , personEmail = "john@local.community"
              , personDob = fromGregorian 1980 6 23
              , password = "supersecure"
}

susan :: Person
susan = Person { personName = "Susan"
               , personEmail = "susan@company.work"
               , personDob = fromGregorian 1990 9 12
               , password = "notReally"
}

persons :: [Person]
persons = [john, susan]

logs :: [LogEntry]
logs = [ LogEntry john (pack "another day, another dollar") $ fromGregorian 2017 10 29
       , LogEntry susan (pack "the dog ran away. john was gonna eat it") $ fromGregorian 2017 10 29
       , LogEntry susan (pack "lunch was good today") $ fromGregorian 2017 10 29
       , LogEntry john (pack "I won 3 rusty cans on the quiz broadcast!") $ fromGregorian 2017 10 22
       , LogEntry john (pack "it's so dark in here") $ fromGregorian 2017 10 22]

-- Now let's describe our REST API to get these logs.  We have:
-- 1. GET /person/{name} which gets you a person's information.
-- 2. GET /persons       gets you a list of all names
-- 3. GET /log/{name}    gets you all logs in chronological order authored by {name}.
--
-- in servant, we create a synonom for a type that describes our API.  
-- each endpoint we build up with the :> combinator, allowing us to specify 
-- when we want a url param, et cetera.  see 
-- http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html#combinators
-- for a list of all possible combinators.
--
-- There is another combinator :<|> which combines two different endpoints.  So 
-- something like
-- "a" :<|> "b" 
-- is a valid haskell type.  
-- The type keyword just puts a name to it.
--
-- The last bit of any endpoint is a HttpVerb ReturnFormat ReturnType.
--
-- Servant handles the encoding for ReturnFormat

type RestApi = "person" :> Capture "name" String :> Get '[JSON] Person
          :<|> "persons" :> Get '[JSON] [Person]
          :<|> "logs" :> Capture "author" String :> Get '[JSON] [LogEntry]  

-- Now we create a Server API.
-- Server a can be thought of as a function that takes an API type and returns 
-- a new type - a Server API, whose implementation consists of functions
-- for each specific endpoint.  
-- The order of these functions must match the order they appear in the API type.
-- The functions are "chained" together with the same :<|> combinator from servant.
-- Each function must return a Handler.  Handler is a new type that much match the 
-- ReturnType of the API endpoint.
--
-- To create a Handler.  Handler is a specific monad for doing our HTML output.
-- we use return :: (Monad m) => a -> m, which apparently can act like a Handler.
-- Servant allows us to change the Monad where our functions run within, but that's
-- a bit deeper than I'm exploring here...
-- see:
-- http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#from-combinators-to-handler-arguments

server :: Servant.Server RestApi
server = getPerson 
    :<|> getAllPersons
    :<|> getLogs
    where getPerson :: String -> Handler Person
          getPerson name = return . head $ filter (\p -> map toLower (personName p) == name) persons

          getAllPersons :: Handler [Person]
          getAllPersons = return persons

          getLogs :: String -> Handler [LogEntry]
          getLogs author = return $ filter (\l -> map toLower (personName $ logAuthor l) == author) logs

-- I don't actually know what this does, but it seems to be important.
-- I don't even understand the syntax.

restApi :: Servant.Proxy RestApi
restApi = Servant.Proxy

-- Now let's head over to app/Main.hs to run this thing. 
