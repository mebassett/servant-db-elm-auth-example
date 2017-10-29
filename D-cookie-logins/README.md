# D-login-cookies

Cookie-based authentication on the haskell rest api and the elm front end. Uses servant-auth.

Users of our app can no longer see the logs from all survivors, or even their basic info.  First you must log in as a survivor, then you can see your own stats and logs.

Servant-auth implements a Cookie-to-header token (https://en.wikipedia.org/wiki/Cross-site_request_forgery#Cookie-to-header_token) to prevent XSRF attacks.  This required some changes in both elm and haskell to support.

## Start with src/Api.hs.

Here we've broken up the REST Api type into authenticated (protected) and non-authenticated (unprotected) parts.  

We've implemented a server unique to each half the API spec, and combined them with a combinator just like we do Api types.

We're using Person as our login token, so we've implemented a JWT interface for it.

There is some servant-auth magic happening in the checkLogin function.  It uses the acceptLogin function to create a handler that includes login cookie and XSRF token headers.  

There is another example of postgresql-simple in the getLoginFromDb function.

## Then head to app/Main.hs.

Not much here, but notice how the server function has changed - we're using cookie and jwt settings.
We've also added logging to stdout.  This is not strictly necessary or part of this example.

## Now to elm-code-generation/Main.hs.

servant-elm doesn't support authentication combinators or header combinators out of the box, so we've done a lot of haskell magic here.

To get it to create a Elm request for a NoContent with Headers endpoint, I've implemented a new ElmType for the Header combinator. It ignores the actual header content.  In our case its only for the browser.

To handle Authenticated combinators, we've injected a header into each endpoint of the API.  This header contains the XSRF token.  Notice that the cookie name in app/Main.hs must match the argName on line59. 

## Now to elm/src/Main.hs, elm/src/Native/CsrfCookie.js, elm/src/CsrfCookie.elm

Main.hs has changed to include a login request, and some view state changes depending on when you are logged in or not.

the CsrfCookie files come straight from https://github.com/mchaver/servant-auth-and-elm-example/blob/master/elm/src/Main.elm . They are here to figure out the right XSRF token.  
elm/src/Main.hs has a sendWithCsrfToken function that takes a (string -> Request) function defined from servant-elm and sends the request after injecting the token determined by CsrfCookie. 

# To Run

$ make && make run

Point your browser to http://localhost:8081.

It is more difficult to debug with haskell and elm separately because csrf policies don't work when you're authenticating.

# usage

Point your browser to http://localhost:8081.
