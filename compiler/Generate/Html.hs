{-# LANGUAGE OverloadedStrings #-}
module Generate.Html (generate, JSSource(..)) where

import qualified Data.Text.Lazy as T
import Text.Blaze (preEscapedToMarkup)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

data JSSource = Link String | Source T.Text

makeScript :: JSSource -> H.Html
makeScript source =
    case source of
      Link src -> H.script ! A.type_ "text/javascript" ! A.src (H.toValue src) $ ""
      Source src ->
          H.script ! A.type_ "text/javascript" $
           preEscapedToMarkup $ src

generate :: FilePath -> String -> [JSSource] -> String -> String -> H.Html
generate libLoc title scripts moduleName noscript =
    H.docTypeHtml $ do 
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ title
        makeScript (Link libLoc)
        mapM_ makeScript scripts
      H.body $ do
        H.script ! A.type_ "text/javascript" $
               preEscapedToMarkup ("Elm.fullscreen(Elm." ++ moduleName ++ ")")
        H.noscript $ preEscapedToMarkup noscript
