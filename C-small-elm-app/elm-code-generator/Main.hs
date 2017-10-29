module Main where

-- This file generates Elm code for the API endpoints, including
-- JSON decoders and definitions for our Haskell types that
-- we are sending through.
--
-- I haven't looked too deep into the stuff from elm-export (the
-- Elm module) but it seems fairly straightforward.  I don't understand
-- the Proxy stuff here, either, however.

import Data.Proxy (Proxy (Proxy))

import Elm (Spec (Spec), specsToDir, toElmTypeSource,
            toElmDecoderSource, toElmEncoderSource, ElmType)
import Servant.Elm (ElmOptions (..), defElmImports, defElmOptions,
                    generateElmForAPIWith, UrlPrefix (Static) )
import Api (Person, LogEntry, RestApiElm)
import Text.PrettyPrint.Leijen.Text
import Data.Text

instance ElmType Person
instance ElmType LogEntry


-- by default servant-elm doesn't expose Date.
-- it doesn't detect that its using date and expose it either.
-- we include this to get valid Elm.
ourElmImports :: Text
ourElmImports = append defElmImports  $ 
                Data.Text.unlines [ "import Date exposing (..)"
                                  , "import Exts.Json.Decode exposing (decodeDate)"]

-- not strictly needed but for debugging this is handy.  See CORS in app/Main.hs
ourElmOpts :: ElmOptions
ourElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8081"}

specs :: [Spec]
specs = [ Spec ["Generated","RestApi"]
            (ourElmImports 
             : toElmTypeSource (Proxy :: Proxy Person)
             : toElmDecoderSource (Proxy :: Proxy Person)
             : toElmTypeSource (Proxy :: Proxy LogEntry)
             : toElmDecoderSource (Proxy :: Proxy LogEntry)
             : generateElmForAPIWith ourElmOpts (Proxy :: Proxy RestApiElm))]

main :: IO ()
main = specsToDir specs "elm/src"
