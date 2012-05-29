{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

import Data.Text (Text)
import Language.Elm
import Language.Elm.Yesod
import Yesod
--import Yesod.Default.Util
import Text.Hamlet
import Text.Julius

data ElmTest = ElmTest

-- embedding an external elm file (note: no spaces!)
mousePage = [elmFile|elm_source/mouse.elm|]

rootPage mouse clock shapes = [elmFile|elm_source/index.elm|]

clockPage = [elmFile|elm_source/clock.elm|]

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

-- generateWidget is called with the result of an Elm QuasiQuoter (which is just a 
-- string containing some Elm code, with proper newline escaping)
getMouseR :: Handler RepHtml
getMouseR = defaultLayout $ do
    setTitle "Mouse position demo"
    elmWidget mousePage

getClockR :: Handler RepHtml
getClockR = defaultLayout $ do
    setTitle "A clock"
    elmWidget clockPage

getShapesR :: Handler RepHtml
getShapesR = defaultLayout $ do
    setTitle "Simple shapes"
    elmWidget shapesPage

-- URLs are rendered manually and then passed on to the function containing
-- the elm QuasiQuoter.
getRootR :: Handler RepHtml
getRootR = do
    render <- getUrlRender
    defaultLayout $ do
      let mouse = render MouseR
          clock = render ClockR
          shapes = render ShapesR
      setTitle "Welcome!"
      elmWidget $ rootPage mouse clock shapes


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