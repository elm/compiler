{- | This module exports the functions necessary for compiling Elm code into the
     respective HTML, JS and CSS code.

     It also provides a predefined compileToHtml function for use with the Blaze markup library
     as well as a simple QuasiQuoter for embedding literal elm code in a Haskell file.

     The documentation for the Elm language is available at <http://elm-lang.org/Documentation.elm>
-}
module Language.Elm (
    toHtml,
    toParts) where

import CompileToJS
import ExtractNoscript
import GenerateHtml
import Initialize
import Text.Blaze.Html (Html)

-- |This function compiles Elm code into a full HTML page.
--
--  Usage example:
--
-- > toHtml "/elm-min.js" "Some title" [elmFile|elm-source/somePage.elm|]
toHtml :: String -- ^ Location of elm-min.js as expected by the browser
       -> String -- ^ The page title
       -> String -- ^ The elm source code.
       -> Html
toHtml = generateHtml

-- |This function compiles Elm code to three separate parts: HTML, CSS,
--  and JavaScript. The HTML is only the contents of the body, so the three
--  parts must be combined in a basic HTML skeleton.
toParts :: String -- ^ The Elm source code.
        -> (Html, Html, String) -- ^ HTML, CSS, and JavaScript in that order.
toParts source = (html, css, js)
    where expr = initialize source
          js = compileToJS expr
          html = body $ either id extract expr
      