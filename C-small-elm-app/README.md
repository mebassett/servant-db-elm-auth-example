# C-small-elm-app

Introduces an Elm frontend to the Haskell-servant API.

The haskell server serves static html generated from elm.

The Haskell is essentially the same as before except with some minor additions for:
1. Making debug easier, if you want to run the elm from `elm-reactor` and run the backend separately
   You'll see this in app/Main.hs
2. Serving static html.  You'll see this in src/Api.hs

This also includes Elm api and data structure code generation from servant-elm.
This is in elm-code-generation/Main.hs.
That file is pretty straight-forward.

# To Run

$ make && make run

Point your browser to http://localhost:8081.

If you want to do `elm-reactor` and haskell separately (much better for development) 
you can do 
$ cd elm && elm-reactor

Then start the haskell (in another process) as before with
$ stack exec exe

To do just code generation, do
$ stack exec code-gen

# usage

Point your browser to http://localhost:8081.
