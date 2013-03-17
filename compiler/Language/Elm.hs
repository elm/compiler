{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- | This module exports the functions necessary for compiling Elm code into the
     respective HTML, JS and CSS code.

     The type class @'ElmSource'@ requires an instance for all types that the Elm
     compiler understands. The provided instances for String, Text and QuasiQuoted
     Elm source code should be sufficient.

     The documentation for the Elm language is available at
     <http://elm-lang.org/Documentation.elm>, and many interactive examples are
     available at <http://elm-lang.org/Examples.elm>

     Example implementations using Yesod and Happstack are available
     at <https://github.com/tazjin/Elm/tree/master/Examples>
-}
module Language.Elm (
    ElmSource (..),
    runtimeLocation
    ) where

import Data.Version (showVersion)
import CompileToJS
import ExtractNoscript
import GenerateHtml
import Initialize
import Text.Blaze.Html (Html)
import Language.Elm.Quasi
import Paths_Elm

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

-- |The absolute path to Elm's runtime system.
runtimeLocation :: IO FilePath
runtimeLocation = getDataFileName "elm-runtime.js"

class ElmSource a where
  -- |This function compiles Elm code to three separate parts: HTML, CSS,
  --  and JavaScript. The HTML is only the contents of the body, so the three
  --  parts must be combined in a basic HTML skeleton.
  compile :: a -> String

  -- |This function compiles Elm code into a full HTML page.
  toHtml :: String -- ^ Location of elm-min.js as expected by the browser
         -> String -- ^ The page title
         -> a      -- ^ The elm source code
         -> Html

instance ElmSource String where
  compile = compileHelper
  toHtml = generateHtml

instance ElmSource Elm where
  compile = compileHelper . TL.unpack . renderElm
  toHtml elmL title  = generateHtml elmL title  . TL.unpack . renderElm

-- |Strict text
instance ElmSource TS.Text where
  compile = compileHelper . TS.unpack
  toHtml elmL title = generateHtml elmL title . TS.unpack

-- |Lazy text
instance ElmSource TL.Text where
  compile = compileHelper . TL.unpack
  toHtml elmL title = generateHtml elmL title . TL.unpack

-- | (urlRenderFn, urlRenderFn -> Elm)
instance ElmSource (t, t -> Elm) where
  compile (f, s) = compileHelper $ TL.unpack $ renderElm $ s f  
  toHtml elmL title (f, s) = generateHtml elmL title $ TL.unpack $ renderElm $ s f

-- | to be used without URL interpolation
instance ElmSource (t -> Elm) where
  compile s = compileHelper $ TL.unpack $ renderElm $ s undefined
  toHtml l t s = generateHtml l t $ TL.unpack $ renderElm $ s undefined


-- build helper to avoid boilerplate repetition
compileHelper :: String -> String
compileHelper source = either showErr jsModule modul
  where modul = buildFromSource source
