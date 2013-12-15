module Build.Info where

import qualified Paths_Elm as This
import qualified Data.Version as Version

version :: String
version = Version.showVersion This.version

runtime :: IO FilePath
runtime = This.getDataFileName "elm-runtime.js"

