
module Language.Elm where

import CompileToJS
import GenerateHtml
import Text.Blaze.Html (Html)

compileToHtml :: String -> String -> String -> Html
compileToHtml libLoc fileName source =
    generateHtml libLoc fileName (compileToJS source)