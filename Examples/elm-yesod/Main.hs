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

-- elmWidget takes some elm source code and returns the finished elm widget
-- inside the GHandler monad. URL interpolation is done automatically, all
-- interpolated variables have to be in scope when the elmWidget call happens.
getMouseR :: Handler RepHtml
getMouseR = do
    widget <- elmWidget mousePage
    defaultLayout $ do
      setTitle "Mouse position demo"
      widget

getClockR :: Handler RepHtml
getClockR = do 
    widget <- elmWidget clockPage
    defaultLayout $ do
      setTitle "A clock"
      widget

getShapesR :: Handler RepHtml
getShapesR = do
    widget <- elmWidget shapesPage
    defaultLayout $ do
      setTitle "Simple shapes"
      widget

getRootR :: Handler RepHtml
getRootR = do
    widget <- elmWidget $(elmFile "elm_source/index.elm")
    defaultLayout $ do
      setTitle "Welcome!"
      widget


-- Our Yesod instance contains the default layout, which inserts the elm-min.js
-- file in the site's <head> tag.
instance Yesod ElmTest where
    jsLoader _ = BottomOfHeadBlocking
    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            addScriptRemote $ "http://f.cl.ly/items/2e3Z3r3v29263U393c3x/elm-min.js"
            $(whamletFile "templates/default-layout.hamlet")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

main :: IO ()
main = warpDebug 3000 ElmTest