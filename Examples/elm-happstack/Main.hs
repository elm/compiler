{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

import Control.Monad (msum)
import Happstack.Server
import Language.Elm
import Language.Elm.Quasi

elmLoc :: String
elmLoc = "http://f.cl.ly/items/2e3Z3r3v29263U393c3x/elm-min.js"

-- elmResponse is a "nice to have" helper function for compiling
-- Elm code when using Elm with Happstack. At some point this might
-- be moved to an elm-happstack package.
elmResponse :: ElmSource a
            => String -- ^ Page title
            -> a      -- ^ elm source
            -> Response
elmResponse title = toResponse . toHtml elmLoc title

-- embedding variables (in this case URLs)
rootHandler :: ServerPart Response
rootHandler = ok $ elmResponse "Welcome!" $ elmIndex
  where
    mouse = "/mouse"
    clock = "/clock"
    shapes = "/shapes"
    elmIndex = $(elmFile "elm_source/index.elm")

-- loading elm source from file

mouseHandler :: ServerPart Response
mouseHandler = ok $ elmResponse "Mouse position demo" 
                        $(elmFile "elm_source/mouse.elm")

clockHandler :: ServerPart Response
clockHandler = ok $ elmResponse "A clock" $(elmFile "elm_source/clock.elm")

-- embedding elm code inside Haskell using the QuasiQuoter:
shapesPage = [elm|
square   = rect 200 200 (150,150)
circle   = oval 140 140 (150,150)
pentagon = ngon   5  60 (150,150)

main = collage 300 300
         [ outlined black square
         , filled green pentagon
         , customOutline [8,4] blue circle
         ]
|]

shapesHandler :: ServerPart Response
shapesHandler = ok $ elmResponse "Simple shapes" $ shapesPage

-- routing
elmExample :: ServerPart Response
elmExample =
    msum [ nullDir >> rootHandler
         , dir "mouse" $ nullDir >>
              mouseHandler
         , dir "clock" $ nullDir >>
              clockHandler
         , dir "shapes" $ nullDir >>
              shapesHandler
         ]

main :: IO ()
main = simpleHTTP nullConf {port = 3000} elmExample