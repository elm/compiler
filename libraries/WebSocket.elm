module WebSocket where
{-| A library for low latency HTTP communication. See the HTTP library for
standard requests like GET, POST, etc. The API of this library is likely to
change to make it more flexible.

# Open a Connection
@docs connect
-}

import Signal (Signal)
import Native.WebSocket

{-| Create a web-socket. The first argument is the URL of the desired
web-socket server. The input signal holds the outgoing messages,
and the resulting signal contains the incoming ones.
-}
connect : String -> Signal String -> Signal String
connect = Native.WebSocket.connect

-- data Action = Open String | Close String | Send String String
-- connections : Signal Action -> Signal String