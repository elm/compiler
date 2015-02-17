
type alias Point2D =
    Point Float Float

type alias Point a b =
    { x:a, y:b }


input points : Signal Point2D


input pairs : Signal (Point String String)
