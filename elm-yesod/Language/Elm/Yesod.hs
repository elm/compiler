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
module Language.Elm.Yesod (toWidget) where

import Text.Blaze (preEscapedToMarkup)
import Text.Hamlet
import Text.Julius
import Yesod.Widget (toWidgetHead, whamlet, GWidget)
import Language.Elm

-- |toWidget takes some Elm code in String format and produces a widget. Usage example:
-- 
-- > toWidget [elmFile|elm-source/somePage.elm|]
toWidget :: String -- ^ The Elm source code
         -> GWidget sub master ()
toWidget source =
  let (html, css, js) = toParts source in
  do toWidgetHead [hamlet| #{css} |]
     toWidgetHead [julius| #{js} |]
     [whamlet| ^{html} |]
