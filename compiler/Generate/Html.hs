{-# LANGUAGE OverloadedStrings #-}
module Generate.Html (generateHtml,
                      createHtml,
                      JSStyle (..),
                      JSSource (..)
                     ) where

import Data.List (intercalate)
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Text.Jasmine (minify)
import qualified Data.ByteString.Lazy.Char8 as BS

import Initialize (buildFromSource)
import Generate.JavaScript
import Generate.Noscript
import Metadata.Libraries as Libraries

data JSStyle = Minified | Readable
data JSSource = Link String | Source JSStyle String

makeScript :: JSSource -> H.Html
makeScript source =
    case source of
      Link src -> H.script ! A.type_ "text/javascript" ! A.src (H.toValue src) $ ""
      Source style src ->
          H.script ! A.type_ "text/javascript" $
           preEscapedToMarkup $ case style of 
                                  Minified -> BS.unpack . minify . BS.pack $ src
                                  Readable -> src

-- |This function compiles Elm code into simple HTML.
--
--  Usage example:
--
-- > generateHtml "/elm-min.js" "Some title" [elmFile|elm-source/somePage.elm|]
generateHtml :: String -- ^ Location of elm-runtime.js as expected by the browser
             -> String -- ^ The page title
             -> String -- ^ The elm source code.
             -> Html
generateHtml libLoc title source = H.span "broken for now"
{--
  case buildFromSource True source of
    Right modul -> createHtml libLoc title [] "" title [modul]
    Left err -> createHtml Readable libLoc title (Right $ showErr err)
                (H.noscript "") "Main"
--}
{--
modulesToHtml :: FilePath -> String -> []
modulesToHtml libLoc title scripts nscrpt modules =
  createHtml jsStyle libLoc title' js noscript altTitle
    where
      js = Right $ jss ++ concatMap jsModule modules
      noscript = if nscrpt then extractNoscript $ last modules else ""
      title' = if null title then altTitle else title
      altTitle = intercalate "." names
          where Module names _ _ _ = last modules
--}                 

createHtml :: FilePath -> String -> [JSSource] -> String -> Html
createHtml libLoc title scripts noscript =
    H.docTypeHtml $ do 
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ title
        makeScript (Link libLoc)
        mapM_ makeScript scripts
      H.body $ do
        H.script ! A.type_ "text/javascript" $ preEscapedToMarkup ("Elm.fullscreen(Elm.Main)" :: String)
        H.noscript $ preEscapedToMarkup noscript
