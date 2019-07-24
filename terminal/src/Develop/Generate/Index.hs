{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Develop.Generate.Index
  ( generate
  )
  where


import Control.Monad (filterM)
import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Directory as Dir
import System.FilePath ((</>), splitDirectories, takeExtension)

import qualified BackgroundWriter as BW
import qualified Develop.Generate.Help as Help
import qualified Elm.Details as Details
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting
import qualified Stuff



-- GENERATE


generate :: FilePath -> IO B.Builder
generate pwd =
  do  flags <- getFlags pwd
      return $ Help.makePageHtml "Index" (Just (encode flags))



-- FLAGS


data Flags =
  Flags
    { _root :: FilePath
    , _pwd :: [String]
    , _dirs :: [FilePath]
    , _files :: [File]
    , _readme :: Maybe String
    , _outline :: Maybe Outline.Outline
    , _exactDeps :: Map.Map Pkg.Name V.Version
    }


data File =
  File
    { _path :: FilePath
    , _runnable :: Bool
    }



-- GET FLAGS


getFlags :: FilePath -> IO Flags
getFlags pwd =
  do  contents <- Dir.getDirectoryContents pwd
      root <- Dir.getCurrentDirectory
      dirs <- getDirs pwd contents
      files <- getFiles pwd contents
      readme <- getReadme pwd
      outline <- getOutline
      exactDeps <- getExactDeps outline
      return $
        Flags
          { _root = root
          , _pwd = dropWhile ("." ==) (splitDirectories pwd)
          , _dirs = dirs
          , _files = files
          , _readme = readme
          , _outline = outline
          , _exactDeps = exactDeps
          }



-- README


getReadme :: FilePath -> IO (Maybe String)
getReadme dir =
  do  let readmePath = dir </> "README.md"
      exists <- Dir.doesFileExist readmePath
      if exists
        then Just <$> readFile readmePath
        else return Nothing



-- GET DIRECTORIES


getDirs :: FilePath -> [FilePath] -> IO [FilePath]
getDirs pwd contents =
  filterM (Dir.doesDirectoryExist . (pwd </>)) contents



-- GET FILES


getFiles :: FilePath -> [FilePath] -> IO [File]
getFiles pwd contents =
  do  paths <- filterM (Dir.doesFileExist . (pwd </>)) contents
      mapM (toFile pwd) paths


toFile :: FilePath -> FilePath -> IO File
toFile pwd path =
  if takeExtension path == ".elm" then
    do  source <- readFile (pwd </> path)
        let hasMain = List.isInfixOf "\nmain " source
        return (File path hasMain)
  else
    return (File path False)



-- GET OUTLINE


getOutline :: IO (Maybe Outline.Outline)
getOutline =
  do  maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing ->
          return Nothing

        Just root ->
          do  result <- Outline.read root
              case result of
                Left _        -> return Nothing
                Right outline -> return (Just outline)



-- GET EXACT DEPS


-- TODO revamp how `elm reactor` works so that this can go away.
-- I am trying to "just get it working again" at this point though.
--
getExactDeps :: Maybe Outline.Outline -> IO (Map.Map Pkg.Name V.Version)
getExactDeps maybeOutline =
  case maybeOutline of
    Nothing ->
      return Map.empty

    Just outline ->
      case outline of
        Outline.App _ ->
          return Map.empty

        Outline.Pkg _ ->
          do  maybeRoot <- Stuff.findRoot
              case maybeRoot of
                Nothing ->
                  return Map.empty

                Just root ->
                  BW.withScope $ \scope ->
                  do  result <- Details.load Reporting.silent scope root
                      case result of
                        Left _ ->
                          return Map.empty

                        Right (Details.Details _ validOutline _ _ _ _) ->
                          case validOutline of
                            Details.ValidApp _ ->
                              return Map.empty

                            Details.ValidPkg _ _ solution ->
                              return solution



-- ENCODE


encode :: Flags -> E.Value
encode (Flags root pwd dirs files readme outline exactDeps) =
  E.object
    [ "root" ==> encodeFilePath root
    , "pwd" ==> E.list encodeFilePath pwd
    , "dirs" ==> E.list encodeFilePath dirs
    , "files" ==> E.list encodeFile files
    , "readme" ==> maybe E.null E.chars readme
    , "outline" ==> maybe E.null Outline.encode outline
    , "exactDeps" ==> E.dict Pkg.toJsonString V.encode exactDeps
    ]


encodeFilePath :: FilePath -> E.Value
encodeFilePath filePath =
  E.chars filePath


encodeFile :: File -> E.Value
encodeFile (File path hasMain) =
  E.object
    [ "name" ==> encodeFilePath path
    , "runnable" ==> E.bool hasMain
    ]
