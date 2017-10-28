# A-servant-start

This implements a super basic Servant RESTful app a la
http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#from-combinators-to-handler-arguments

Start reading the source code from lib/Api.hs

Packages and language extensions used are in A-servant-start.cabal.

# To Run

$ stack build
$ stack exec exe

Should do it.  

$ curl http://localhost:8081/persons

Should then work.  After reading the source you should be able
to think of a few more examples.
