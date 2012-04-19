{-# LANGUAGE OverloadedStrings #-}
module ToHtml (compileToHtml) where

import CompileToJS
import Data.Monoid (mempty)
import GenerateHtml
import Network.HTTP.Base (urlEncode)
import Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

pageTitle fp =
    reverse . takeWhile (/='/') . drop 1 . dropWhile (/='.') $ reverse fp


compileToHtml :: String -> String -> String -> Html
compileToHtml libLoc fileName source =
    generateHtml libLoc (pageTitle fileName) (compileToJS source)