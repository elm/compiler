
type Point2D = Point Float Float

type Point a b = { x:a, y:b }

port points : Signal Point2D

port pairs : Signal (Point String String)
