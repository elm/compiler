{-# LANGUAGE OverloadedStrings #-}
module GenerateHtml (generateHtml, body, css) where

import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Initialize
import CompileToJS
import ExtractNoscript

css = "* { padding:0; margin:0; \
      \hyphens: auto; -moz-hyphens: auto;\
      \ -webkit-hyphens: auto; -ms-hyphens: auto; }\
      \body { font-family: Arial; }\
      \a:link {text-decoration: none}\
      \a:visited {text-decoration: none}\
      \a:active {text-decoration: none}\
      \a:hover {text-decoration: underline; color: #ff8f12;}"

makeScript :: String -> H.Html
makeScript s = H.script ! A.type_ "text/javascript" ! A.src (H.toValue s) $ ""

-- |This function compiles Elm code into simple HTML.
--
--  Usage example:
--
-- > generateHtml "/elm-min.js" "Some title" [elmFile|elm-source/somePage.elm|]
generateHtml :: String -- ^ Location of elm-min.js as expected by the browser
             -> String -- ^ The page title
             -> String -- ^ The elm source code.
             -> Html
generateHtml libLoc title source =
    let expr = initialize source
        js = compileToJS expr
        noscript = either id extract expr
    in
    H.docTypeHtml $ do 
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ title
        makeScript libLoc
        H.script ! A.type_ "text/javascript" $ preEscapedToMarkup js
        H.style ! A.type_ "text/css" $ preEscapedToMarkup (css :: String)
      H.body $ body noscript

body noscript = do
  H.div ! A.id "widthChecker" ! A.style "width:100%; height:1px; position:absolute; top:-1px;" $ ""
  H.span ! A.id "content" $ ""
  H.script ! A.type_ "text/javascript" $ "Dispatcher.initialize()"
  H.noscript $ preEscapedToMarkup noscript 