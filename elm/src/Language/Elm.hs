{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- | This module exports the functions necessary for compiling Elm code into the
     respective HTML, JS and CSS code.

     It also provides a predefined compileToHtml function for use with the Blaze markup library
     as well as a simple QuasiQuoter for embedding literal elm code in a Haskell file.

     The documentation for the Elm language is available at <http://elm-lang.org/Documentation.elm>
-}
module Language.Elm (
    ElmSource (..)
    ) where

import CompileToJS
import ExtractNoscript
import GenerateHtml
import Initialize
import Text.Blaze.Html (Html)
import Language.Elm.Quasi

import qualified Data.Text.Lazy as TL

class ElmSource a where
  -- |This function compiles Elm code to three separate parts: HTML, CSS,
  --  and JavaScript. The HTML is only the contents of the body, so the three
  --  parts must be combined in a basic HTML skeleton.
  toParts :: a -> (Html, Html, String)
  -- |This function compiles Elm code into a full HTML page.
  toHtml :: String -- ^ Location of elm-min.js as expected by the browser
         -> String -- ^ The page title
         -> a      -- ^ The elm source code
         -> Html

instance ElmSource String where
  toParts = toPartsHelper
  toHtml = generateHtml

instance ElmSource Elm where
  toParts = toPartsHelper . TL.unpack . renderElm
  toHtml elmL title  = generateHtml elmL title  . TL.unpack . renderElm

instance ElmSource TL.Text where
  toParts = toPartsHelper . TL.unpack
  toHtml elmL title = generateHtml elmL title . TL.unpack

instance ElmSource (t -> Elm) where
  toParts s = toPartsHelper $ TL.unpack $ renderElm $ s undefined  
  toHtml elmL title s = generateHtml elmL title $ TL.unpack $ renderElm $ s undefined


-- build helper to avoid boilerplate repetition
toPartsHelper :: String -> (Html, Html, String)
toPartsHelper source = (html, css, js)
  where expr = initialize source
        js = compileToJS expr
        html = body $ either id extract expr
