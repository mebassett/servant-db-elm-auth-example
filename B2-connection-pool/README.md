# B-postgres-data

Modified the earlier example so that it uses PostgreSQL for its tabase

Start reading the source code from lib/Api.hs

added OverloadedStrings to language extensions in the cabal.
This lets Strings become ByteStrings or Querys as needed.

requires postgresql 9.6, and related development packages.
please ensure your OS has them installed.
On fedora using the postgresql96- packages, please note that ghc and gc 
will look for pg_config in /usr/bin/ but it will actually be in
/usr/pg-somethingorrather/bin/.  You'll need to create a sym link.

Most of the changes are in src/Api.hs
but there are some changes to app/Main.hs.  After reading the former
you can probably predict them.

# To Run

$ createdb haskell-stuff
$ psql haskell-stuff < setup.sql
$ stack build
$ stack exec exe

Should do it.  

# usage

same as before.
