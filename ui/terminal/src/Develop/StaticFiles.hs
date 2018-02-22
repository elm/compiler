{-# OPTIONS_GHC -Wall #-}
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
import Data.FileEmbed (bsToExp)
import qualified Data.HashMap.Strict as HM
import Language.Haskell.TH (runIO)

import qualified Develop.StaticFiles.Build as Build



-- FILE LOOKUP


type MimeType =
  String


lookup :: FilePath -> Maybe (BS.ByteString, MimeType)
lookup path =
  HM.lookup path dict


dict :: HM.HashMap FilePath (BS.ByteString, MimeType)
dict =
  HM.fromList
    [ faviconPath  ==> (favicon , "image/x-icon")
    , waitingPath  ==> (waiting , "image/gif")
    , elmPath      ==> (elm     , "application/javascript")
    , cssPath      ==> (css     , "text/css")
    , codeFontPath ==> (codeFont, "font/ttf")
    , sansFontPath ==> (sansFont, "font/ttf")
    ]


(==>) :: a -> b -> (a,b)
(==>) a b =
  (a, b)



-- PATHS


faviconPath :: FilePath
faviconPath =
  "favicon.ico"


waitingPath :: FilePath
waitingPath =
  "_elm/waiting.gif"


elmPath :: FilePath
elmPath =
  "_elm/elm.js"


cssPath :: FilePath
cssPath =
  "_elm/styles.css"


codeFontPath :: FilePath
codeFontPath =
  "_elm/source-code-pro.ttf"


sansFontPath :: FilePath
sansFontPath =
  "_elm/source-sans-pro.ttf"



-- ELM


elm :: BS.ByteString
elm =
  $(bsToExp =<< runIO Build.compile)



-- CSS


css :: BS.ByteString
css =
  $(bsToExp =<< runIO (Build.readAsset "styles.css"))



-- FONTS


codeFont :: BS.ByteString
codeFont =
  $(bsToExp =<< runIO (Build.readAsset "source-code-pro.ttf"))


sansFont :: BS.ByteString
sansFont =
  $(bsToExp =<< runIO (Build.readAsset "source-sans-pro.ttf"))



-- IMAGES


favicon :: BS.ByteString
favicon =
  $(bsToExp =<< runIO (Build.readAsset "favicon.ico"))


waiting :: BS.ByteString
waiting =
  $(bsToExp =<< runIO (Build.readAsset "waiting.gif"))
