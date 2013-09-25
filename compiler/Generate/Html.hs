{-# LANGUAGE OverloadedStrings #-}
module Generate.Html
    (generateHtml,
     createHtml,
     JSStyle (..),
     JSSource (..)
    ) where

import Text.Blaze (preEscapedToMarkup)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Text.Jasmine (minify)
import qualified Data.ByteString.Lazy.Char8 as BS

import Initialize (buildFromSource)
import Generate.JavaScript
import Generate.Noscript

data JSStyle = Minified | Readable
data JSSource = Link String | Source JSStyle BS.ByteString

makeScript :: JSSource -> H.Html
makeScript source =
    case source of
      Link src -> H.script ! A.type_ "text/javascript" ! A.src (H.toValue src) $ ""
      Source style src ->
          H.script ! A.type_ "text/javascript" $
           preEscapedToMarkup $ BS.unpack $ case style of 
                                              Minified -> minify src
                                              Readable -> src

-- |This function compiles Elm code into simple HTML.
--
--  Usage example:
--
-- > generateHtml "/elm-min.js" "Some title" [elmFile|elm-source/somePage.elm|]
generateHtml :: String -- ^ Location of elm-runtime.js as expected by the browser
             -> String -- ^ The page title
             -> String -- ^ The elm source code.
             -> H.Html
generateHtml libLoc title source = error "function 'generateHtml' is unimplemented for now"

createHtml :: FilePath -> String -> [JSSource] -> String -> String -> H.Html
createHtml libLoc title scripts moduleName noscript =
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
