
module Clicks where
import WebSocket
msgs = show <~ count Mouse.clicks
main = asText <~ open "ws://localhost:8080/ws" msgs

