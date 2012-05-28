{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

import Data.Text (Text)
import Language.Elm
import Language.Elm.Yesod
import Yesod
import Yesod.Default.Util
import Text.Hamlet
import Text.Julius

data ElmTest = ElmTest

-- embedding an external elm file (note: no spaces!)
mousePage = [elmFile|elm_source/mouse.elm|]

-- embedding elm code in our Haskell file
rootPage = [elm|
main = plainText "Welcome!"
|]

-- our Yesod App
mkYesod "ElmTest" [parseRoutes|
/ RootR GET
/mouse MouseR GET
|]

-- generateWidget is called with the result of an Elm QuasiQuoter (which is just a 
-- string containing some Elm code, with proper newline escaping)
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