{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

import Language.Elm
import Language.Elm.Quasi
import Language.Elm.Yesod
import Yesod
import Text.Hamlet

data ElmTest = ElmTest

-- loading external elm code
mousePage = $(elmFile "elm_source/mouse.elm")

clockPage = $(elmFile "elm_source/clock.elm")

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

-- our Yesod App
mkYesod "ElmTest" [parseRoutes|
/ RootR GET
/mouse MouseR GET
/clock ClockR GET
/shapes ShapesR GET
|]

-- Your App data type needs to have an instance of YesodElm (see line 75&76)
-- so that toWidget can work with QuasiQuoted elm code. All URL interpolation
-- is done automatically. (e.g. lines 28-30 in elm_source/index.elm)
getMouseR :: Handler RepHtml
getMouseR = 
  defaultLayout $ do
      setTitle "Mouse position demo"
      toWidget mousePage

getClockR :: Handler RepHtml
getClockR =
    defaultLayout $ do
      setTitle "A clock"
      toWidget clockPage

getShapesR :: Handler RepHtml
getShapesR =
    defaultLayout $ do
      setTitle "Simple shapes"
      toWidget shapesPage

getRootR :: Handler RepHtml
getRootR =
    defaultLayout $ do
      setTitle "Welcome!"
      toWidget $(elmFile "elm_source/index.elm")


-- Our Yesod instance contains the default layout, which inserts the elm-min.js
-- file in the site's <head> tag. The YesodElm instance defines the location of
-- elm-min.js

instance Yesod ElmTest where
    jsLoader _ = BottomOfHeadBlocking
    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            $(whamletFile "templates/default-layout.hamlet")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

instance YesodElm ElmTest where
  urlElmJs _ = Right $ "https://raw.github.com/evancz/Elm/master/elm/elm-runtime-0.3.0.js"

main :: IO ()
main = warpDebug 3000 ElmTest