module Http where
{-| A library for asynchronous HTTP requests. See the `WebSocket`
library if you have very strict latency requirements.

# Sending Requests
@docs send, sendGet, sendJsonp

# Creating Requests
@docs get, post, request

# Responses
@docs Response, map
-}

import Signal (..)
import Native.Http

import Json
import JavaScript.Experimental as JS

{-| The datatype for responses. `Success` contains only the returned message.
`Failure`s contain both an error code and an error message.
-}
data Response a = Success a | Waiting | Failure Int String

{-| Apply a function to the contents of a `Response`, if it's a `Success`. -}
map : (a -> b) -> Response a -> Response b
map f req =
  case req of
    Success x -> Success (f x)
    Waiting -> Waiting
    Failure status error -> Failure status error

type Request a = {
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

{-| Performs a JSONP request with the given urls (set callback parameter
to "?"). Produces a signal that carries the responses.
-}
sendJsonp : Signal String -> Signal (Response Json.Value)
sendJsonp urls = map JS.toJson <~ Native.Http.sendJsonp urls
