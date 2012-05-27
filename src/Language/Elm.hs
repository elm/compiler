
module Language.Elm where

import GenerateHtml
import Text.Blaze.Html (Html)

-- | The 'compileToHtml' function takes three string arguments: the
-- location of the Elm runtime (elm-mini.js), the title to be used in
-- the resulting HTML page, and the Elm source code. For example,
-- 
-- > compileToHtml "/elm-mini.js" "Hello, World!" "main = plainText \"Hello, World!\""

compileToHtml :: String -> String -> String -> Html
compileToHtml libLoc fileName source =
    generateHtml libLoc fileName source