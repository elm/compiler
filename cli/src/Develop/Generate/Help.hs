{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Develop.Generate.Help
  ( makeHtml
  , makeCodeHtml
  , makeElmHtml
  )
  where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Develop.StaticFiles as StaticFiles



-- PAGES


makeHtml :: String -> String -> String -> H.Html
makeHtml title jsFile initCode =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title $ H.toHtml title
      H.link
        ! A.type_ "text/css"
        ! A.rel "stylesheet"
        ! A.href (H.toValue ("/" ++ StaticFiles.cssPath))
      H.script ! A.src (H.toValue jsFile) $ ""

    H.body $ do
      H.script $ H.preEscapedToMarkup initCode



-- CODE


makeCodeHtml :: String -> String -> H.Html
makeCodeHtml title code =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title $ H.toHtml title
      H.style ! A.type_ "text/css" $ codeStyle

      H.link ! A.rel "stylesheet" ! A.href "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/styles/default.min.css"
      H.script ! A.src "//cdnjs.cloudflare.com/ajax/libs/highlight.js/9.3.0/highlight.min.js" $ ""
      H.script $ "if (hljs) { hljs.initHighlightingOnLoad(); }"

    H.body ! A.style "background-color: #F0F0F0;" $ do
      H.pre $ H.code $ H.toHtml code


codeStyle :: H.Html
codeStyle =
  H.toHtml $ unlines $
    [ "html, head, body, pre {"
    , "  margin: 0;"
    , "  height: 100%;"
    , "}"
    , "body {"
    , "  font-family: 'Source Code Pro', monospace;"
    , "}"
    ]



-- ELM CODE


makeElmHtml :: FilePath -> H.Html
makeElmHtml filePath =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title $ H.toHtml ("~/" ++ filePath)
      H.style ! A.type_ "text/css" $ elmStyle

    H.body $ do
      H.div ! A.style waitingStyle $ do
        H.div ! A.style "font-size: 3em;" $ "Building your project!"
        H.img ! A.src (H.toValue StaticFiles.waitingPath)
        H.div ! A.style "font-size: 1em" $ "With new projects, I need a bunch of extra time to download packages."

    H.script ! A.src (H.toValue ("/_compile/" ++ filePath)) ! A.charset "utf-8" $ ""
    H.script $ H.preEscapedToMarkup $ unlines $
      [ "while (document.body.firstChild) {"
      , "    document.body.removeChild(document.body.firstChild);"
      , "}"
      , "runElmProgram();"
      ]


elmStyle :: H.Html
elmStyle =
  H.toHtml $ unlines $
    [ "@import url(http://fonts.googleapis.com/css?family=Source+Sans+Pro);"
    , "html, head, body {"
    , "  margin: 0;"
    , "  height: 100%;"
    , "}"
    ]


waitingStyle :: H.AttributeValue
waitingStyle =
  H.stringValue $
    "width: 100%; height: 100%; display: flex; flex-direction: column;"
    ++ " justify-content: center; align-items: center; color: #9A9A9A;"
    ++ " font-family: 'Source Sans Pro';"
