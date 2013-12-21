import Debug.Logging
import Mouse

main = (\numClicks->asText <| Debug.Logging.unsafePrintLogMessage numClicks) <~ count Mouse.clicks