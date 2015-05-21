

type alias Point2D =
    Point Float Float

type alias Point x y =
    { x:x, y:y }


port points : Point2D


port pairs : Point String String
