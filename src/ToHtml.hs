{-# LANGUAGE OverloadedStrings #-}
module ToHtml (compileToHtml) where

import CompileToJS
import GenerateHtml
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

pageTitle fp =
    reverse . takeWhile (/='/') . drop 1 . dropWhile (/='.') $ reverse fp

compileToHtml :: String -> String -> String -> H.Html
compileToHtml libLoc fileName source =
    generateHtml libLoc (pageTitle fileName) (compileToJS source)