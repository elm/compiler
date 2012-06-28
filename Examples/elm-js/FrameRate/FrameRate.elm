
module FrameRate where

import Data.Maybe (mapMaybe)
import Foreign.JavaScript
import Signal.Keyboard.Raw
import Signal.Input


foreign import jsevent "trigger" (castIntToJSNumber 0)
  jsTime :: Signal JSNumber

foreign export jsevent "finished"
  done :: Signal JSBool

foreign export jsevent "desiredFPS"
  desiredFPS :: Signal JSNumber

desiredFPS = constant (castIntToJSNumber 30)

time = lift castJSNumberToFloat jsTime


-- Calculate FPS based on incoming times.

getFPS ts =
  let len = length ts in
  if len > 1 then (len-1) / (head ts - last ts) else 0
truncate n = castJSNumberToInt (castFloatToJSNumber $ 100 * n) / 100
fps = lift (truncate . getFPS) (foldp (\t ts -> take 10 (t:ts)) [0] time)


-- Use time and keyboard input to determine the
-- position of the black square.

addVecs (x1,y1) (x2,y2) = (x1+x2, y1+y2)
scaleBy s (x,y) = (x*s, y*s)

addKey k (x,y) =
  if k < 37 || k > 40 then (x,y) else
    if k `mod` 2 == 1 then (x + k - 38, y)
                      else (x, y + k - 39)

velocity = lift (foldl addKey (0,0)) keysDown

timestep = lift snd $ foldp (\t1 (t0,d) -> (t1, t1-t0)) (0,0) time

delta = dropIf (\(x,y) -> x == 0 && y == 0) (0,0) $
        lift (scaleBy 100) (lift2 scaleBy timestep velocity)

position = foldp addVecs (0,0) delta


-- Display moving square and FPS on screen.

screen pos actual desired =
  flow down [ collage 400 400 [ outlined black (rect 40 40 pos) ]
            , plainText "Move the square around with the arrow keys."
            , text $ toText "Actual frames per second: " ++ show actual
            , text $ toText "Desired frames per second: " ++ show desired
            ]


game = lift3 screen position fps desiredFPS
done = lift (\_ -> castBoolToJSBool True) game

main = game