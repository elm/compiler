
type alias Thing =
    { x:Float
    , y:Float
    }


f : Thing -> Thing
f t =
  let x = t.x
      y = t.y
  in  t
