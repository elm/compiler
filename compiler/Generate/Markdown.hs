module Generate.Markdown where

import qualified Cheapskate as CS
import qualified Cheapskate.Html as CS
import qualified Data.Text as Text
import qualified Text.Blaze.Html.Renderer.String as Blaze
import qualified Text.Highlighting.Kate as Kate

(|>) :: a -> (a -> b) -> b
x |> f = f x

toHtml :: String -> String
toHtml rawMarkdown =
    Text.pack rawMarkdown
        |> CS.markdown options
        |> CS.walk highlightCode
        |> CS.renderDoc
        |> Blaze.renderHtml

options :: CS.Options
options =
    CS.Options
    { CS.sanitize = False
    , CS.allowRawHtml = True
    , CS.preserveHardBreaks = False
    , CS.debug = False
    }

highlightCode :: CS.Block -> CS.Block
highlightCode block =
    case block of
      CS.CodeBlock (CS.CodeAttr lang _info) src ->
        Kate.highlightAs (Text.unpack lang) (Text.unpack src)
            |> Kate.formatHtmlBlock Kate.defaultFormatOpts
            |> Blaze.renderHtml
            |> Text.pack
            |> CS.HtmlBlock

      _ -> block
