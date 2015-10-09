
type alias Thing =
    { x : Float
    , y : Float
    }


bindFields : Thing -> Thing
bindFields thing =
  let
    x = thing.x
    y = thing.y
  in
    thing
