{-# LANGUAGE OverloadedStrings #-}
module GenerateHtml (generateHtml,
                     body, css, widgetBody,
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
import Initialize
import CompileToJS
import ExtractNoscript

css = H.style ! A.type_ "text/css" $ preEscapedToMarkup
      ("html,head,body { padding:0; margin:0; }\
       \body { font-family: helvetica, arial, sans-serif; }" :: String)

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
generateHtml :: String -- ^ Location of elm-min.js as expected by the browser
             -> String -- ^ The page title
             -> String -- ^ The elm source code.
             -> Html
generateHtml libLoc title source =
    case initialize source of
      Left err -> createHtml Readable libLoc title (Right $ showErr err) (H.noscript "")
      Right (escs, modul) -> modulesToHtml Readable title libLoc [] True [(escs,modul)]


modulesToHtml jsStyle title libLoc jss nscrpt pairs =
    createHtml jsStyle libLoc title' js noscript
    where modules = map snd pairs
          js = Right $ jss ++ concatMap jsModule pairs
          noscript = if nscrpt then extractNoscript $ last modules else ""
          title' = if null title then altTitle else title
          altTitle = (\(Module names _ _ _) -> intercalate "." names) $
                     last modules
                  

linkedHtml rtLoc jsLoc modules =
    createHtml Readable rtLoc title (Left jsLoc) (H.noscript "")
    where title = (\(Module names _ _ _) -> intercalate "." names) $
                  snd (last modules)


createHtml jsStyle libLoc title js noscript =
    H.docTypeHtml $ do 
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ title
        css
      H.body $ do
        makeScript Readable (Left libLoc)
        makeScript jsStyle js
        body noscript

body noscript = do
  H.div ! A.id "widthChecker" ! A.style "width:100%; height:1px; position:absolute; top:-1px;" $ ""
  H.div ! A.id "content" $ ""
  H.script ! A.type_ "text/javascript" $ "Dispatcher.initialize()"
  H.noscript $ preEscapedToMarkup noscript

widgetBody noscript = do
  H.div ! A.id "widthChecker" ! A.style "width:100%; height:1px; position:absolute; top:-1px;" $ ""
  H.div ! A.id "content" $ ""
  H.noscript $ preEscapedToMarkup noscript
