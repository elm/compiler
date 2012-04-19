{-# LANGUAGE OverloadedStrings #-}
module GenerateHtml (generateHtml) where

import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

css = preEscapedString "* { padding:0; margin:0; \
                       \hyphens: auto; -moz-hyphens: auto;\
                       \ -webkit-hyphens: auto; -ms-hyphens: auto; }\
                       \body { font-family: Arial; }\
                       \a:link {text-decoration: none}\
                       \a:visited {text-decoration: none}\
                       \a:active {text-decoration: none}\
                       \a:hover {text-decoration: underline; color: #ff8f12;}"
                       
makeScript :: String -> Html
makeScript s = script ! type_ "text/javascript" ! src (toValue s) $ ""

generateHtml libLoc title source =
    docTypeHtml $ do 
      H.head $ do
        meta ! charset "UTF-8"
        H.title . toHtml $ title
        makeScript libLoc
        (script ! type_ "text/javascript") . preEscapedString $ source
        H.style ! type_ "text/css" $ css
      body $ do
        H.div ! A.id "widthChecker" ! A.style "width:100%; height:1px; position:absolute; top:-1px;" $ ""
        H.span ! A.id "content" $ ""
        script ! type_ "text/javascript" $ "Dispatcher.initialize()"
