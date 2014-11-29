
type alias Point2D =
    Point Float Float

type alias Point a b =
    { x:a, y:b }


port points : Signal Point2D


port pairs : Signal (Point String String)
