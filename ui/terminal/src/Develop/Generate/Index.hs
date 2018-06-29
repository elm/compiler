{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Develop.Generate.Index
  ( get
  )
  where


import Control.Monad (filterM)
import qualified Data.ByteString.Builder as B
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Directory as Dir
import System.FilePath ((</>), splitDirectories, takeExtension)

import qualified Develop.Generate.Help as Help
import qualified Elm.Package as Pkg
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Summary as Summary
import qualified Json.Encode as E
import qualified Reporting.Progress as Progress
import qualified Reporting.Task as Task
import qualified Stuff.Verify as Verify



-- GET


get :: FilePath -> FilePath -> IO B.Builder
get root pwd =
  do  flags <- getFlags root pwd
      return $ Help.makePageHtml "Index" (Just (encode flags))



-- FLAGS


data Flags =
  Flags
    { _root :: FilePath
    , _pwd :: [String]
    , _dirs :: [FilePath]
    , _files :: [(FilePath, Bool)]
    , _readme :: Maybe Text.Text
    , _project :: Maybe Project.Project
    , _exactDeps :: Map.Map Pkg.Name Pkg.Version
    }



-- JSON


encode :: Flags -> E.Value
encode (Flags root pwd dirs files readme project exactDeps) =
  E.object
    [ ( "root", encodeFilePath root )
    , ( "pwd", E.list encodeFilePath pwd )
    , ( "dirs", E.list encodeFilePath dirs )
    , ( "files", E.list encodeFile files )
    , ( "readme", maybe E.null E.text readme )
    , ( "project", maybe E.null Project.encode project )
    , ( "exactDeps", E.dict Pkg.toText Pkg.encodeVersion exactDeps)
    ]


encodeFilePath :: FilePath -> E.Value
encodeFilePath filePath =
  E.text (Text.pack filePath)


encodeFile :: (FilePath, Bool) -> E.Value
encodeFile (file, hasMain) =
  E.object
    [ ("name", encodeFilePath file)
    , ("runnable", E.bool hasMain)
    ]



-- GET FLAGS


getFlags :: FilePath -> FilePath -> IO Flags
getFlags root pwd =
  do  (dirs, files) <- getDirsAndFiles pwd
      readme <- getReadme pwd
      exists <- Dir.doesFileExist (root </> "elm.json")

      maybeSummary <-
        if exists then
          Task.try Progress.silentReporter $
            Verify.verify root =<< Project.read (root </> "elm.json")
        else
          return Nothing

      return $
        Flags
          { _root = root
          , _pwd = dropWhile ("." ==) (splitDirectories pwd)
          , _dirs = dirs
          , _files = files
          , _readme = readme
          , _project = fmap Summary._project maybeSummary
          , _exactDeps = maybe Map.empty (Map.map fst . Summary._depsGraph) maybeSummary
          }


getReadme :: FilePath -> IO (Maybe Text.Text)
getReadme dir =
  do  let readmePath = dir </> "README.md"
      exists <- Dir.doesFileExist readmePath
      if exists
        then Just <$> Text.readFile readmePath
        else return Nothing


getDirsAndFiles :: FilePath -> IO ([FilePath], [(FilePath, Bool)])
getDirsAndFiles pwd =
  do  contents <- Dir.getDirectoryContents pwd
      dirs <- filterM (Dir.doesDirectoryExist . (pwd </>)) contents
      filePaths <- filterM (Dir.doesFileExist . (pwd </>)) contents
      files <- mapM (inspectFile pwd) filePaths
      return (dirs, files)


inspectFile :: FilePath -> FilePath -> IO (FilePath, Bool)
inspectFile pwd path =
  if takeExtension path == ".elm" then
    do  source <- Text.readFile (pwd </> path)
        let hasMain = Text.isInfixOf "\nmain " source
        return (path, hasMain)

  else
    return (path, False)
