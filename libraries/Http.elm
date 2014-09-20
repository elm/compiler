module Http where
{-| A library for asynchronous HTTP requests. See the `WebSocket`
library if you have very strict latency requirements.

# Sending Requests
@docs send, sendGet

# Creating Requests
@docs get, post, request

# Responses
@docs Response
-}

import Signal (..)
import Native.Http

{-| The datatype for responses. Success contains only the returned message.
Failures contain both an error code and an error message.
-}
type Response a
    = Success a
    | Waiting
    | Failure Int String

type alias Request a = {
  verb : String,
  url  : String,
  body : a,
  headers : [(String,String)]
 }

{-| Create a customized request. Arguments are request type (get, post, put,
delete, etc.), target url, data, and a list of additional headers.
-}
request : String -> String -> String -> [(String,String)] -> Request String
request = Request

{-| Create a GET request to the given url. -}
get : String -> Request String
get url = Request "GET" url "" []

{-| Create a POST request to the given url, carrying the given data. -}
post : String -> String -> Request String
post url body = Request "POST" url body []

{-| Performs an HTTP request with the given requests. Produces a signal
that carries the responses.
-}
send : Signal (Request a) -> Signal (Response String)
send = Native.Http.send

{-| Performs an HTTP GET request with the given urls. Produces a signal
that carries the responses.
-}
sendGet : Signal String -> Signal (Response String)
sendGet reqs = send (lift get reqs)
