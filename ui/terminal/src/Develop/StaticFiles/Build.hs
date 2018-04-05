{-# LANGUAGE OverloadedStrings #-}
module Develop.StaticFiles.Build
  ( readAsset
  , compile
  )
  where


import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Elm.Compiler.Objects as Obj
import qualified Elm.Project as Project
import qualified Generate.Output as Output
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- ASSETS


readAsset :: FilePath -> IO BS.ByteString
readAsset path =
  BS.readFile ("ui" </> "browser" </> "assets" </> path)



-- COMPILE


compile :: IO BS.ByteString
compile =
  Dir.withCurrentDirectory ("ui" </> "browser") $
    do  reporter <- Terminal.create
        void $ Task.run reporter $
          do  summary <- Project.getRoot
              Project.compile options Nothing summary rootPaths

        result <- BS.readFile tempFileName
        seq (BS.length result) (Dir.removeFile tempFileName)
        return result


options :: Output.Options
options =
  Output.Options Obj.Prod Obj.Client $
    Just (Output.JavaScript Nothing tempFileName)


tempFileName :: FilePath
tempFileName =
  "elm.js"


rootPaths :: [FilePath]
rootPaths =
  [ "src" </> "Errors.elm"
  , "src" </> "Index.elm"
  , "src" </> "NotFound.elm"
  ]
