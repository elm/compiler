module Develop.Generate.NotFound where

import qualified Text.Blaze.Html5 as H

import qualified Develop.StaticFiles as StaticFiles
import qualified Develop.Generate.Help as Help


html :: H.Html
html =
  Help.makeHtml
    "Page Not Found"
    ("/" ++ StaticFiles.elmPath)
    "Elm.NotFound.fullscreen();"
