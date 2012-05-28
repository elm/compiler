{-# LANGUAGE QuasiQuotes #-}
{- | This module provides a function for compiling Elm source code into a Yesod widget.
     In order to use this with your Yesod app, you need to define a defaultLayout like
     function that embeds the elm-min.js file /in the <head> tag/.

     For example, you could modify your Yesod instance as follows:

   > instance Yesod App where
   >    jsLoader _ = BottomOfHeadBlocking -- moves JS to the <head> tag
   >    defaultLayout widget = do
   >        pc <- widgetToPageContent $ do
   >            addScriptRemote $ "http://somecdn.org/link/to/elm-min.js"
   >            ...
   >        ...

     A full example implementation is provided in the examples folder of the Elm github repository.
-}
module Language.Elm.Yesod (generateWidget) where

import Text.Blaze (preEscapedToMarkup)
import Text.Hamlet
import Text.Julius
import Text.Lucius
import Yesod.Widget

import Language.Elm.Initialize
import Language.Elm.CompileToJS
import Language.Elm.ExtractNoscript

css = [lucius|
* { padding:0; margin:0;
hyphens: auto; -moz-hyphens: auto;
-webkit-hyphens: auto; -ms-hyphens: auto;}
body { font-family: Arial; }
a:link, a:visited, a:active { text-decoration: none}
a:hover {text-decoration: underline; color: #ff8f12;}
|]

-- |generateWidget takes some Elm code in String format and produces a widget. Usage example:
-- 
-- > generateWidget [elmFile|elm-source/somePage.elm|]
generateWidget :: String -- ^ The Elm source code
               -> GWidget sub master ()
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
