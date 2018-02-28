{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Develop.Generate.Index
  ( toHtml
  , getProject
  , moveToRoot
  )
  where

import Control.Monad (filterM)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.UTF8 as Utf8
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Directory as Dir
import System.FilePath ((</>), joinPath, splitDirectories, takeExtension)
import qualified Text.Blaze.Html5 as H

import qualified Develop.Generate.Help as Help
import qualified Develop.StaticFiles as StaticFiles
import qualified Json.Encode as E



-- PROJECT


data Project =
  Project
    { _root :: FilePath
    , _info :: PerhapsInfo
    }


data PerhapsInfo
  = Bad (Maybe FilePath)
  | Good Info


data Info =
  Info
    { _pwd :: [String]
    , _dirs :: [FilePath]
    , _files :: [(FilePath, Bool)]
    , _readme :: Maybe Text.Text
    , _config :: Text.Text
    }



-- JSON


goodToJson :: FilePath -> Info -> String
goodToJson root (Info pwd dirs files readme config) =
  Utf8.toString $ B.toLazyByteString $ E.encodeUgly $
    E.object
      [ ( "root", encodeFilePath root )
      , ( "pwd", E.list encodeFilePath pwd )
      , ( "dirs", E.list encodeFilePath dirs )
      , ( "files", E.list encodeFile files )
      , ( "readme", maybe E.null E.text readme )
      , ( "config", E.text config )
      ]


encodeFilePath :: FilePath -> E.Value
encodeFilePath filePath =
  E.text (Text.pack filePath)


encodeFile :: (FilePath, Bool) -> E.Value
encodeFile (file, hasMain) =
  E.array [ encodeFilePath file, E.bool hasMain ]


badJson :: FilePath -> Maybe FilePath -> String
badJson root potentialRoot =
  Utf8.toString $ B.toLazyByteString $ E.encodeUgly $
    E.object
      [ ( "root", encodeFilePath root )
      , ( "suggestion", maybe E.null encodeFilePath potentialRoot )
      ]



-- GENERATE HTML


toHtml :: Project -> H.Html
toHtml (Project root perhapsInfo) =
  case perhapsInfo of
    Good info@(Info pwd _ _ _ _) ->
      Help.makeHtml
        (List.intercalate "/" ("~" : pwd))
        ("/" ++ StaticFiles.elmPath)
        ("Elm.Index.fullscreen(" ++ goodToJson root info ++ ");")

    Bad suggestion ->
      Help.makeHtml
        (maybe "New Project!" (const "Existing Project?") suggestion)
        ("/" ++ StaticFiles.elmPath)
        ("Elm.Start.fullscreen(" ++ badJson root suggestion ++ ");")



-- GET PROJECT


getProject :: FilePath -> IO Project
getProject directory =
  do  root <- Dir.getCurrentDirectory
      exists <- Dir.doesFileExist "elm.json"
      Project root <$>
        case exists of
          False ->
            Bad <$> findNearestConfig (splitDirectories root)

          True ->
            do  json <- Text.readFile "elm.json"
                Good <$> getInfo json directory


findNearestConfig :: [String] -> IO (Maybe FilePath)
findNearestConfig dirs =
  if null dirs then
    return Nothing

  else
    do  exists <- Dir.doesFileExist (joinPath dirs </> "elm.json")
        if exists
          then return (Just (joinPath dirs))
          else findNearestConfig (init dirs)


moveToRoot :: IO ()
moveToRoot =
  do  subDir <- Dir.getCurrentDirectory
      maybeRoot <- findNearestConfig (splitDirectories subDir)
      case maybeRoot of
        Nothing ->
          return ()

        Just root ->
          Dir.setCurrentDirectory root



-- GET INFO


getInfo :: Text.Text -> FilePath -> IO Info
getInfo config directory =
  do  (dirs, files) <- getDirectoryInfo directory
      readme <- getReadme directory
      return $
        Info
          { _pwd = dropWhile ("." ==) (splitDirectories directory)
          , _dirs = dirs
          , _files = files
          , _readme = readme
          , _config = config
          }



-- README


getReadme :: FilePath -> IO (Maybe Text.Text)
getReadme dir =
  do  let readmePath = dir </> "README.md"
      exists <- Dir.doesFileExist readmePath
      if exists
        then Just <$> Text.readFile readmePath
        else return Nothing



-- DIRECTORIES / FILES


getDirectoryInfo :: FilePath -> IO ([FilePath], [(FilePath, Bool)])
getDirectoryInfo dir =
  do  contents <- Dir.getDirectoryContents dir

      allDirs <- filterM (Dir.doesDirectoryExist . (dir </>)) contents
      rawFiles <- filterM (Dir.doesFileExist . (dir </>)) contents

      files <- mapM (inspectFile dir) rawFiles

      return (allDirs, files)


inspectFile :: FilePath -> FilePath -> IO (FilePath, Bool)
inspectFile dir filePath =
  if takeExtension filePath == ".elm" then
    do  source <- Text.readFile (dir </> filePath)
        let hasMain = Text.isInfixOf "\nmain " source
        return (filePath, hasMain)

  else
    return (filePath, False)
