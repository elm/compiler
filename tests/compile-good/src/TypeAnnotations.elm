module TypeAnnotations exposing (..)

identity : a -> a
identity x = x

flip : (a -> b -> c) -> b -> a -> c
flip f b a = f a b

type alias Point =
  { x : Float
  , y : Float
  }

origin : Point
origin = { x = 0, y = 0 }

moveX : Float -> Point -> Point
moveX dx point = { point | x = point.x + dx }
