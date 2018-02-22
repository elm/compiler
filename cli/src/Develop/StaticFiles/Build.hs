{-# OPTIONS_GHC -Wall #-}
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
import qualified Elm.Project.Flags as Flags
import qualified Reporting.Task as Task
import qualified Reporting.Progress.Terminal as Terminal



-- ASSETS


readAsset :: FilePath -> IO BS.ByteString
readAsset path =
  BS.readFile ("ui" </> "assets" </> path)



-- COMPILE


compile :: IO BS.ByteString
compile =
  if True then return "TODO" else
  do  Dir.setCurrentDirectory "ui"

      reporter <- Terminal.create
      void $ Task.run reporter $
        do  summary <- Project.getRoot
            Project.compile options summary rootPaths

      result <- BS.readFile tempFileName
      seq (BS.length result) (Dir.removeFile tempFileName)

      Dir.setCurrentDirectory ".."

      return result


options :: Flags.Options
options =
  Flags.Options Obj.Prod Obj.Client $
    Just (Flags.Custom Nothing tempFileName)


tempFileName :: FilePath
tempFileName =
  "elm.js"


rootPaths :: [FilePath]
rootPaths =
  [ "src" </> "Errors.elm"
  , "src" </> "Index.elm"
  , "src" </> "Start.elm"
  , "src" </> "NotFound.elm"
  ]
