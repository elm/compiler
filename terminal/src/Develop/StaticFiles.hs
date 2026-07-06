{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Develop.StaticFiles
  ( lookup
  , cssPath
  , elmPath
  , waitingPath
  )
  where

import Prelude hiding (lookup)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import System.FilePath ((</>))

import qualified Literals

import qualified Develop.StaticFiles_TH as TH



-- FILE LOOKUP


type MimeType =
  BS.ByteString


lookup :: FilePath -> Maybe (BS.ByteString, MimeType)
lookup path =
  HM.lookup path dict


dict :: HM.HashMap FilePath (BS.ByteString, MimeType)
dict =
  HM.fromList
    [ faviconPath  ==> (favicon , "image/x-icon")
    , elmPath      ==> (elm     , "application/javascript")
    , cssPath      ==> (css     , "text/css")
    , codeFontPath ==> (codeFont, "font/ttf")
    , sansFontPath ==> (sansFont, "font/ttf")
    ]


(==>) :: a -> b -> (a,b)
(==>) a b =
  (a, b)



-- PATHS


faviconPath  :: FilePath; faviconPath  = "favicon.ico"
waitingPath  :: FilePath; waitingPath  = "_elm" </> "waiting.gif"
elmPath      :: FilePath; elmPath      = "_elm" </> "elm.js"
cssPath      :: FilePath; cssPath      = "_elm" </> "styles.css"
codeFontPath :: FilePath; codeFontPath = "_elm" </> "source-code-pro.ttf"
sansFontPath :: FilePath; sansFontPath = "_elm" </> "source-sans-pro.ttf"



-- ASSETS


elm      :: BS.ByteString; elm      = $(TH.buildReactorFrontEnd)
css      :: BS.ByteString; css      = $(Literals.file ("reactor" </> "assets" </> "styles.css"))
codeFont :: BS.ByteString; codeFont = $(Literals.file ("reactor" </> "assets" </> "source-code-pro.ttf"))
sansFont :: BS.ByteString; sansFont = $(Literals.file ("reactor" </> "assets" </> "source-sans-pro.ttf"))
favicon  :: BS.ByteString; favicon  = $(Literals.file ("reactor" </> "assets" </> "favicon.ico"))

