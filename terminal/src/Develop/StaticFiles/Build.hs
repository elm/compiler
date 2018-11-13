{-# LANGUAGE OverloadedStrings #-}
module Develop.StaticFiles.Build
  ( readAsset
  , compile
  )
  where


import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))

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
        Task.run reporter $
          do  summary <- Project.getRoot
              let jsOutput = Just (Output.JavaScript Nothing tempFileName)
              Project.compile Output.Prod Output.Client jsOutput Nothing summary rootPaths

        result <- BS.readFile tempFileName
        seq (BS.length result) (Dir.removeFile tempFileName)
        return result


tempFileName :: FilePath
tempFileName =
  "elm.js"


rootPaths :: [FilePath]
rootPaths =
  [ "src" </> "Errors.elm"
  , "src" </> "Index.elm"
  , "src" </> "NotFound.elm"
  ]
