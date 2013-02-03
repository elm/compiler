
module Mario where

import Dict
import JavaScript
import JSON
import Random
import WebSocket

{- INPUT -}

jumpStep isJump obj = if isJump && obj.y == 0 then { obj | vy <- 5 } else obj
gravityStep t obj = { obj | vy <- if obj.y > 0 then obj.vy - t/4 else obj.vy }
timeStep t obj = let {x,y,vx,vy} = obj in
                 { obj | x <- x + t * vx , y <- max 0 $ y + t * vy }
walkStep dir obj = { obj | vx <- dir, dir <- if | dir < 0   -> "left"
                                                | dir > 0   -> "right"
                                                | otherwise -> obj.dir }

step t d j = timeStep t . gravityStep t . jumpStep j . walkStep d

delta = lift (flip (/) 20) (fps 25)
leftRight = toFloat . .x <~ Keyboard.arrows
jump = (\{y} -> y > 0) <~ Keyboard.arrows
steps = sampleOn delta (lift3 step delta leftRight jump)

{- LOCAL STATE -}

initialMario = { x = 0, y = 0, vx = 0, vy = 0, dir = "right" }
stateSignal = foldp ($) initialMario steps

encode obj id = 
  castJSStringToString . (toPrettyJSString "") . JSON.fromList $ 
    [ ("id", JsonNumber id)
    , ("x", JsonNumber obj.x)
    , ("y", JsonNumber obj.y)
    , ("vx", JsonNumber obj.vx)
    , ("dir", JsonString obj.dir) ]
--encode obj id = show id ++ " " ++ show obj.x ++ " " ++ show obj.y

clientID = inRange 0 99999
myStream = encode <~ stateSignal ~ clientID

{- NETWORK LAYER -}

worldMessageStream = open "ws://localhost:8080/ws" myStream

-- :: String -> Maybe (Float, Record)
parsePlayer msg = 
    case fromString msg of
        Nothing -> Nothing
        Just json ->
                  let id = findNumber "id" json
                      x = findNumber "x" json
                      y = findNumber "y" json
                      vx = findNumber "vx" json
                      dir = findString "dir" json
                  in Just (id, { x = x, y = y, vx = vx, vy = 0, dir = dir })

-- :: Maybe (Float, Record) -> Dict String Record -> Dict String Record
updateWorldPositions maybeMario marioDict = case maybeMario of
  Just (id, mario) -> (Dict.insert) (show id) mario marioDict
  Nothing          -> marioDict

-- :: Signal (Dict String Record)
worldPositions = foldp updateWorldPositions Dict.empty (parsePlayer <~ worldMessageStream)
--worldPositions = constant empty

marios = Dict.values <~ worldPositions

{- RENDER CODE -}

-- :: Record -> Form
mario2Form (w,h) mario = 
  let verb = if mario.vx /= 0 then "walk" else "stand"
      src  = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in toForm (mario.x, (h-63)-mario.y) (image 35 35 src)

-- :: (Int,Int) -> [Record] -> Element
render (w,h) marios =
     collage w h ( (filled cyan $ rect w h (w `div` 2, h `div` 2))
                 : (filled green $ rect w 50 (w `div` 2,h-25))
                 : List.map (mario2Form (w,h)) marios )

{- PUTTING IT TOGETHER -}

-- :: Signal Element
main = render <~ Window.dimensions ~ marios
--main = above <~ ((plainText . show) <~ (marios)) ~ (render <~ Window.dimensions ~ marios)
--main = (plainText . show) <~ (marios)

