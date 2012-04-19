
module TypeHints (typeHints) where

import qualified Data.Map as Map
import Types

elementT = ADT "Element" []
n2e2e = LambdaT IntT $ LambdaT elementT elementT

str2elem = map (\f -> (f, LambdaT StringT elementT))
           [ "image", "video", "text", "leftText", "centerText", "rightText" ]

textAttrs =
    [ ("link", LambdaT StringT s2s), ("fontSize", LambdaT IntT s2s) ] ++
    map (\f -> (f, s2s))
            ["header", "italic", "bold", "underline"
            , "overline", "strikeThrough" ]
        where s2s = LambdaT StringT StringT

allHints = str2elem ++ textAttrs ++
           [ ("flowDown", LambdaT (ADT "List" [elementT]) elementT)
           , ("flowRight", LambdaT (ADT "List" [elementT]) elementT)
           , ("opacity", n2e2e)
           , ("width", n2e2e)
           , ("height", n2e2e)
           , ("size", LambdaT IntT n2e2e)
           , ("centerX", LambdaT elementT elementT)
           ]

typeHints = Map.fromList allHints