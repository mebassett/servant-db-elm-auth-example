module Main where

import Data.Proxy (Proxy (Proxy))
import Elm (Spec (Spec), specsToDir, toElmTypeSource,
            toElmDecoderSource, toElmEncoderSource, toElmType, ElmDatatype, ElmType(..))
import Servant.Elm (ElmOptions (..), defElmImports, defElmOptions,
                    generateElmForAPIWith, UrlPrefix (Static),ElmType )
import Api (Person, LogEntry, RestApiElm, Login)
import Text.PrettyPrint.Leijen.Text
import Data.Text

-- new imports:
import Servant.Foreign (HasForeignType, typeFor, HasForeign, foreignFor,
         GenerateList, generateList, Foreign, Req(..), Arg(..), HeaderArg(..),
         PathSegment(..))
import Servant
import Servant.Elm.Internal.Foreign (LangElm, getEndpoints)
import Servant.Auth.Server


instance ElmType Person
instance ElmType LogEntry
instance ElmType Login

-- One of our new API points includes a list of headers.
-- servant-elm doesn't support this out of the box, because it didn't
-- create an instance of HasForeign from servant-foreign for the header
-- combinator. 
--
-- that's ok, we can create it here.  
--
-- to get this code right, we used quite a few Haskell language exts.  Namely:
--
--                FlexibleInstances
--                MultiParamTypeClasses
--                FlexibleContexts
--                TypeFamilies
--                ScopedTypeVariables
--                TypeOperators

instance (ElmType a) => ElmType (Headers hs a) where
    toElmType _ = toElmType (undefined :: a)

-- the `([1] a) => .. a` syntax was new to me.
-- it means that the type a is an instance of whatever was in [1].

-- servant-auth very smartly provides some protection against XSRF in the form
-- of a cookie-to-header token.  This means our authenticated endpoints must
-- return that same token.  So for our authenticated end points we don't
-- want raw elm requests, we want a function that takes that token and returns
-- a request with the right header.
--
-- servant-elm doesn't play with servant-auth by default, but again we can 
-- fix it by implementing another HasForeign instance for authenticated points.
-- While we do that we'll also tell servant-elm to add in a header.
-- servant-elm knows enough to create the function from that.

authHeader :: HeaderArg ElmDatatype
authHeader = HeaderArg { _headerArg = Arg { _argName = PathSegment "X_XSRF_TOKEN"
                                          , _argType = toElmType $ pack "ElmType"}}

instance (HasForeign LangElm ElmDatatype api) =>
    HasForeign LangElm ElmDatatype (Auth auths types :> api) where
    type Foreign ElmDatatype (Auth auths types :> api) = Foreign ElmDatatype api

    foreignFor lang ftype Proxy req =
        foreignFor lang ftype (Proxy :: Proxy api) newReq
        where newReq = req { _reqHeaders = [authHeader]}

ourElmImports :: Text
ourElmImports = append defElmImports  $ 
                Data.Text.unlines [ "import Date exposing (..)"
                                  , "import Exts.Json.Decode exposing (decodeDate)"]

ourElmOpts :: ElmOptions
ourElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8081"}

specs :: [Spec]
specs = [ Spec ["Generated","RestApi"]
            (ourElmImports 
             : toElmTypeSource (Proxy :: Proxy Person) 
             : toElmTypeSource (Proxy :: Proxy NoContent) -- stuff servant-elm should do.
             : toElmDecoderSource (Proxy :: Proxy Person)
             : toElmTypeSource (Proxy :: Proxy LogEntry)
             : toElmDecoderSource (Proxy :: Proxy LogEntry)
             : toElmTypeSource (Proxy :: Proxy Login)
             : toElmDecoderSource (Proxy :: Proxy Login)
             : toElmEncoderSource (Proxy :: Proxy Login)
             : generateElmForAPIWith ourElmOpts (Proxy :: Proxy (RestApiElm auths)))]

main :: IO ()
main = specsToDir specs "elm/src"
