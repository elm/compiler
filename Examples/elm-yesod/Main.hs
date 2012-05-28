{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

import Data.Text (Text)
import Language.Elm
import Language.Elm.Yesod
import Yesod
import Yesod.Default.Util
import Text.Hamlet
import Text.Julius

data ElmTest = ElmTest

-- our Elm code
mousePage = [elm|
niceBlue   = rgb    0   (1/3) (2/3)
clearGreen = rgba (1/9) (8/9) (3/9) (1/2)

scene (x,y) (w,h) =
  collage w h [ filled niceBlue . rotate ((x+y)/1000) $ ngon 4 100 (200,200)
              , filled clearGreen $ ngon 5 30 (x,y)
              ]

main = lift2 scene Mouse.position Window.dimensions
|]

rootPage = [elm|
main = plainText "Welcome!"
|]

-- our Yesod App

mkYesod "ElmTest" [parseRoutes|
/ RootR GET
/mouse MouseR GET
|]

getMouseR :: Handler RepHtml
getMouseR = defaultLayout $ do
    setTitle "Mouse position demo"
    generateWidget mousePage


getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle "Welcome!"
    generateWidget rootPage

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