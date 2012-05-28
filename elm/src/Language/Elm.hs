{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{- | This module re-exports the modules necessary for compiling Elm code into the
     respective HTML, JS and CSS code.

     It also provides a predefined generateHtml function for use with the Blaze markup library
     as well as a simple QuasiQuoter for embedding literal elm code in a Haskell file.
-}
module Language.Elm (
    module Language.Elm.Initialize,
    module Language.Elm.CompileToJS,
    module Language.Elm.ExtractNoscript,
    generateHtml,
    elm,
    elmFile) where

import Language.Elm.CompileToJS
import Language.Elm.ExtractNoscript
import Language.Elm.Initialize
import Language.Elm.GenerateHtml
import Language.Haskell.TH
import Language.Haskell.TH.Quote

{- | This QuasiQuoter allows for literal embedding of elm source code within
     Haskell files. It is not responsible for the actual compilation Process.
     Usage:

   @
     elmPage = [elm|
      main = image \"someImage.jpg\"
     |]
   @

     How the elm code is compiled depends on how you intend to use it.
     Example uses are the included 'generateHtml' function as well as the
     "Language.Elm.Yesod" module.
-}
elm :: QuasiQuoter
elm = QuasiQuoter {quoteExp = \s -> [|s|] }

{- | elmFile is a quoteFile wrapper around the 'elm' QuasiQuoter and allows for
     external elm source files to be embedded.
     Usage:

   @
     [elmFile|elm-source/page.elm|]
   @

     Please note that no spaces should be added before and after the filename.
-}
elmFile :: QuasiQuoter
elmFile = quoteFile elm