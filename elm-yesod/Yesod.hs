{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Language.Elm.Yesod (generateWidget) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Text.Blaze (preEscapedToMarkup)
import Text.Hamlet
import Text.Julius
import Text.Lucius
import Yesod.Widget

import Initialize
import CompileToJS
import ExtractNoscript

css = [lucius|
* { padding:0; margin:0;
hyphens: auto; -moz-hyphens: auto;
-webkit-hyphens: auto; -ms-hyphens: auto;}
body { font-family: Arial; }
a:link, a:visited, a:active { text-decoration: none}
a:hover {text-decoration: underline; color: #ff8f12;}
|]

generateWidget :: String -> GWidget sub master ()
generateWidget source =
  let expr = initialize source
      js = compileToJS expr
      noscript = either id extract expr
  in do toWidgetHead css
        toWidgetHead [julius| #{js} |]
        [whamlet|
<div #widthChecker style="width:100%; height:1px; position:absolute; top:-1px;">
<span #content>
<script type="text/javascript">
 Dispatcher.initialize()
<noscript>^{preEscapedToMarkup noscript}
|]
