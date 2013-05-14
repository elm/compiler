{-# LANGUAGE OverloadedStrings #-}
module GenerateHtml (generateHtml,
                     modulesToHtml, linkedHtml,
                     JSStyle (..)
                    ) where

import Data.List (intercalate)
import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Text.Jasmine (minify)
import qualified Data.ByteString.Lazy.Char8 as BS

import Ast
import Initialize (buildFromSource)
import CompileToJS
import ExtractNoscript
import Libraries as Libraries

data JSStyle = Minified | Readable

makeScript :: JSStyle -> Either String String -> H.Html
makeScript _ (Left s) =
    H.script ! A.type_ "text/javascript" ! A.src (H.toValue s) $ ""
makeScript jsStyle (Right s) =
    H.script ! A.type_ "text/javascript" $ preEscapedToMarkup content
    where content = case jsStyle of 
                      Minified -> BS.unpack . minify . BS.pack $ s
                      Readable -> s

-- |This function compiles Elm code into simple HTML.
--
--  Usage example:
--
-- > generateHtml "/elm-min.js" "Some title" [elmFile|elm-source/somePage.elm|]
generateHtml :: String -- ^ Location of elm-runtime.js as expected by the browser
             -> String -- ^ The page title
             -> String -- ^ The elm source code.
             -> Html
generateHtml libLoc title source =
  case buildFromSource True source of
    Right modul -> modulesToHtml Readable title libLoc [] True [modul]
    Left err -> createHtml Readable libLoc title (Right $ showErr err)
                (H.noscript "") "Main"


modulesToHtml jsStyle title libLoc jss nscrpt modules =
  createHtml jsStyle libLoc title' js noscript altTitle
    where
      js = Right $ jss ++ concatMap jsModule modules
      noscript = if nscrpt then extractNoscript $ last modules else ""
      title' = if null title then altTitle else title
      altTitle = intercalate "." names
          where Module names _ _ _ = last modules
                  

linkedHtml rtLoc jsLoc modules =
    createHtml Readable rtLoc title (Left jsLoc) (H.noscript "") title
    where
      title = (\(Module names _ _ _) -> intercalate "." names) (last modules)


createHtml jsStyle libLoc title js noscript moduleToLoad =
    H.docTypeHtml $ do 
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ title
        makeScript Readable (Left libLoc)
        makeScript jsStyle js
      H.body $ do
        H.script ! A.type_ "text/javascript" $ preEscapedToMarkup ("Elm.init(Elm." ++ moduleToLoad ++ ")" :: String)
        H.noscript $ preEscapedToMarkup noscript
