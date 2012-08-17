
module Pong where

import Foreign.JavaScript
import Signal.Keyboard.Raw
import Signal.Window as Win


------------------------------------------------------------------------
------            Extracting timesteps from JavaScript            ------
------------------------------------------------------------------------

desiredFPS = constant (castIntToJSNumber 30)

foreign export jsevent "desiredFPS"
  desiredFPS :: Signal JSNumber

foreign import jsevent "trigger" (castIntToJSNumber 0)
  jsTime :: Signal JSNumber

time = lift castJSNumberToFloat jsTime

delta = lift snd $ foldp (\t1 (t0,d) -> (t1, t1-t0)) (0,0) time


------------------------------------------------------------------------
------                  Modelling user input                      ------
------------------------------------------------------------------------

-- Each paddle can be moving up, down, or not at all. We'll call this
-- the `direction' of the paddle.

data Direction = Up | Neutral | Down

-- During gameplay, all keyboard input is about the position of the two
-- paddles. So the keyboard input can be reduced to two `Directions'.

data KeyInput = KeyInput Bool Direction Direction


-- Now we determine how to update the direction of a paddle based on
-- keyboard input. The first two args of `updatePaddle` are the key
-- codes of the up and down keys.

updateDirection upKey downKey key direction =
  case direction of
  { Up      -> if key == downKey then Neutral else Up
  ; Down    -> if key == upKey   then Neutral else Down
  ; Neutral -> if key == upKey   then Up   else
               if key == downKey then Down else Neutral
  }

updateDirection1 = updateDirection 87 83 -- 'w' for up and 's' for down
updateDirection2 = updateDirection 38 40 -- 'UP' for up and 'DOWN' for down

updateInput key (KeyInput space dir1 dir2) =
  KeyInput (space || key == 32)
    (updateDirection1 key dir1)
    (updateDirection2 key dir2)

keyInput = lift (foldl updateInput (KeyInput False Neutral Neutral)) keysDown


------------------------------------------------------------------------
------              Combining all inputs to game                  ------
------------------------------------------------------------------------

-- The inputs to this game include a timestep (which we extracted from
-- JavaScript) and the keyboard input from the users.

data Input = Input Float KeyInput

input = lift2 Input delta keyInput


------------------------------------------------------------------------
------            Modelling Pong / a State Machine                ------
------------------------------------------------------------------------

data Paddle = Paddle Float                      -- y-position
data Ball   = Ball (Float,Float) (Float,Float)  -- position and velocity

data Score = Score Int Int
data State = Play | BetweenRounds

data GameState = GameState State Score Ball Paddle Paddle

gameWidth  = 600
gameHeight = 400
halfWidth  = gameWidth  / 2
halfHeight = gameHeight / 2

defaultGame = GameState BetweenRounds
                        (Score 0 0)
                        (Ball (halfWidth, halfHeight) (150,150))
                        (Paddle halfHeight)
                        (Paddle halfHeight)

stepPaddle delta dir (Paddle y) =
  case dir of
  { Up      -> Paddle . clamp 20 (gameHeight-20) $ y - 200 * delta
  ; Down    -> Paddle . clamp 20 (gameHeight-20) $ y + 200 * delta
  ; Neutral -> Paddle y
  }

stepBall delta (Ball (x,y) (vx,vy)) (Paddle y1) (Paddle y2) =
  let { makePositive n = if n > 0 then n else 0-n
      ; makeNegative n = if n > 0 then 0-n else n
      ; near epsilon n x = x > n - epsilon && x < n + epsilon
      ; vx' = if near 20 y1 y && near 8 25 x
                  then makePositive vx else
              if near 20 y2 y && near 8 (gameWidth - 25) x
                  then makeNegative vx else vx
      ; vy' = if y < 7 then makePositive vy else
              if y > gameHeight - 7 then makeNegative vy else vy
      ; scored = x > gameWidth || x < 0
      ; x' = if scored then halfWidth  else x + vx' * delta
      ; y' = if scored then halfHeight else y + vy' * delta
      }
  in ( Ball (x',y') (vx',vy')
     , if x > gameWidth then 1 else 0
     , if x < 0 then 1 else 0
     )


stepGame (Input delta (KeyInput space dir1 dir2))
         (GameState state (Score s1 s2) ball paddle1 paddle2) =
  let { (ball',s1',s2') = if state == Play
                             then stepBall delta ball paddle1 paddle2
                             else (ball, 0, 0)
      ; state' = case state of { Play -> if s1' /= s2' then BetweenRounds else state
                               ; BetweenRounds -> if space then Play else state }
      }
  in GameState state'
               (Score (s1+s1') (s2+s2'))
               ball'
               (stepPaddle delta dir1 paddle1)
               (stepPaddle delta dir2 paddle2)

gameState = foldp stepGame defaultGame input



display (w,h) (GameState state (Score p1 p2) (Ball pos _) (Paddle y1) (Paddle y2)) =
  let score = width w . centeredText . Text.height 4 $
              show p1 ++ toText "    " ++ show p2
  in  layers
    [ if state == Play then score else
        score `above` (width w . centeredText $ toText "Press SPACE to begin.")
    , let pongGreen = rgb 60 100 60 in
      size w h . box 5 $ collage gameWidth gameHeight
        [ filled pongGreen (rect gameWidth gameHeight (halfWidth,halfHeight))
        , filled white (oval 15 15 pos)
        , filled white (rect 10 40 (            20, y1))
        , filled white (rect 10 40 (gameWidth - 20, y2))
        ]
    ]

view = lift2 display Win.dimensions gameState

done = lift (\_ -> castBoolToJSBool True) view
foreign export jsevent "finished"
  done :: Signal JSBool

main = view