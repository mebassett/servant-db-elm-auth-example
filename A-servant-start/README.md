# A-servant-start

This implements a super basic `servant` RESTful app a la [`servant` tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#from-combinators-to-handler-arguments).

Start by reading the source code from in [`src/Api.hs`](https://github.com/mebassett/servant-db-elm-auth-example/blob/master/A-servant-start/src/Api.hs).

Packages and language extensions used are in [`A-servant-start.cabal`](https://github.com/mebassett/servant-db-elm-auth-example/blob/master/A-servant-start/A-servant-start.cabal).

## To Run

```sh
$ stack setup
$ stack build
$ stack exec exe
```

Should do it. There is no `stdout` message once the server is ready to receive its first HTTP request.

## Usage

```sh
$ curl http://localhost:8081/persons
```

Should then work.

After reading the source you should be able to think of a few more examples.
