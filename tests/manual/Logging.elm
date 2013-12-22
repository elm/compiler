import Debug.Logging
import Mouse

v : Int
v = Debug.Logging.unsafePrintLogMessage "Hey!" 1

b : Int
b = Debug.Logging.unsafeShowLabledValueInLog "b:" 2

main = (\numClicks->asText <| Debug.Logging.unsafeShowValueInLog numClicks) <~ count Mouse.clicks
