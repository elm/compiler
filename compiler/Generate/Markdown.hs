module Generate.Markdown where

import Text.Pandoc

toHtml :: String -> String
toHtml = writeHtmlString def . readMarkdown def