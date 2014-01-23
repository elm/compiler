module Http where

{-| A library for asynchronous HTTP requests. See the
[WebSocket](http://elm-lang.org/docs/WebSocket.elm) library if
you have very strict latency requirements.

# Sending Requests
@docs send, sendGet

# Creating Requests
@docs get, post, request

# Responses
@docs Response
-}

import open Signal
import Native.Http
import Dict

type Headers = Dict.Dict String String

{-| The datatype for responses. Success contains the returned message and the
headers. Failures contain an error code, the message and the headers.
-}
data Response a = Success a Headers
                | Waiting
                | Failure Int String Headers

type Request a = {
  verb : String,
  url  : String,
  body : a,
  headers : Headers
 }

{-| Create a customized request. Arguments are request type (get, post, put,
delete, etc.), target url, data, and a list of additional headers.
-}
request : String -> String -> String -> Headers -> Request String
request = Request

{-| Helper method to create a request using a list instead of a dict for
headers
-}
request' : String -> String -> String -> [(String,String)] -> Request String
request' verb url body headers = Request verb url body (Dict.fromList headers)

{-| Sets a header in the request. Adds it if it does not exists, replaces its
value if it exists
-}
setHeader : (String, String) -> Request a -> Request a
setHeader (header, value) request = { request | headers <- Dict.insert header value (request.headers) }

{-| Create a GET request to the given url. -}
get : String -> Request String
get url = Request "GET" url "" Dict.empty

{-| Create a POST request to the given url, carrying the given data. -}
post : String -> String -> Request String
post url body = Request "POST" url body Dict.empty

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
