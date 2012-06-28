{-# LANGUAGE OverloadedStrings #-}
module GenerateHtml (generateHtml,
                     body, css, widgetBody,
                     modulesToHtml, linkedHtml
                    ) where

import Data.List (intercalate)
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Ast
import Initialize
import CompileToJS
import ExtractNoscript

css = H.style ! A.type_ "text/css" $ preEscapedToMarkup
      ("* { padding:0; margin:0; \
       \hyphens: auto; -moz-hyphens: auto;\
       \ -webkit-hyphens: auto; -ms-hyphens: auto; }\
       \body { font-family: Arial; }\
       \a:link {text-decoration: none}\
       \a:visited {text-decoration: none}\
       \a:active {text-decoration: none}\
       \a:hover {text-decoration: underline; color: #ff8f12;}" :: String)

makeScript :: Either String String -> H.Html
makeScript (Left s) =
    H.script ! A.type_ "text/javascript" ! A.src (H.toValue s) $ ""
makeScript (Right s) = 
    H.script ! A.type_ "text/javascript" $ preEscapedToMarkup s

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
    case initialize source of
      Left err -> createHtml libLoc title (Right $ showErr err) (H.noscript "")
      Right modul -> modulesToHtml title libLoc [] [modul]


modulesToHtml title libLoc jss modules = createHtml libLoc title' js noscript
    where js = Right $ jss ++ concatMap jsModule modules
          noscript = extract $ last modules
          title' = if null title then altTitle else title
          altTitle = (\(Module names _ _ _ _) -> intercalate "." names) $
                     last modules
                  

linkedHtml rtLoc jsLoc modules =
    createHtml rtLoc title (Left jsLoc) (H.noscript "")
    where title = (\(Module names _ _ _ _) -> intercalate "." names) $
                  last modules


createHtml libLoc title js noscript =
    H.docTypeHtml $ do 
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ title
        makeScript (Left libLoc)
        makeScript js
        css
      H.body $ body noscript

body noscript = do
  H.div ! A.id "widthChecker" ! A.style "width:100%; height:1px; position:absolute; top:-1px;" $ ""
  H.div ! A.id "content" $ ""
  H.script ! A.type_ "text/javascript" $ "Dispatcher.initialize()"
  H.noscript $ preEscapedToMarkup noscript

widgetBody noscript = do
  H.div ! A.id "widthChecker" ! A.style "width:100%; height:1px; position:absolute; top:-1px;" $ ""
  H.div ! A.id "content" $ ""
  H.noscript $ preEscapedToMarkup noscript
