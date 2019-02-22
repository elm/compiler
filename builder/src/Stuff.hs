{-# OPTIONS_GHC -Wall #-}
module Stuff
  ( details
  , interfaces
  , objects
  , prepublishDir
  , elmi
  , elmo
  , temp
  , getRoot
  , PackageCache
  , getPackageCache
  , registry
  , package
  , getReplCache
  )
  where


import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V



-- PATHS


stuff :: FilePath
stuff =
  "elm-stuff" </> compilerVersion


details :: FilePath
details =
  stuff </> "d.dat"


interfaces :: FilePath
interfaces =
  stuff </> "i.dat"


objects :: FilePath
objects =
  stuff </> "o.dat"


prepublishDir :: FilePath
prepublishDir  =
  stuff </> "prepublish"


compilerVersion :: FilePath
compilerVersion =
  V.toChars V.compiler



-- ELMI and ELMO


elmi :: FilePath -> ModuleName.Raw -> FilePath
elmi root name =
  toArtifactPath root name "elmi"


elmo :: FilePath -> ModuleName.Raw -> FilePath
elmo root name =
  toArtifactPath root name "elmo"


toArtifactPath :: FilePath -> ModuleName.Raw -> String -> FilePath
toArtifactPath root name ext =
  root </> stuff </> ModuleName.toHyphenPath name <.> ext



-- TEMP


temp :: String -> FilePath
temp ext =
  stuff </> "temp" <.> ext



-- GET ROOT


getRoot :: IO (Maybe FilePath)
getRoot =
  do  dir <- Dir.getCurrentDirectory
      getRootHelp (FP.splitDirectories dir)


getRootHelp :: [String] -> IO (Maybe FilePath)
getRootHelp dirs =
  case dirs of
    [] ->
      return Nothing

    _:_ ->
      do  exists_ <- Dir.doesFileExist (FP.joinPath dirs </> "elm.json")
          if exists_
            then return (Just (FP.joinPath dirs))
            else getRootHelp (init dirs)



-- PACKAGE CACHES


newtype PackageCache = PackageCache FilePath


getPackageCache :: IO PackageCache
getPackageCache =
  PackageCache <$> getCacheDir "packages"


registry :: PackageCache -> FilePath
registry (PackageCache dir) =
  dir </> "registry.dat"


package :: PackageCache -> Pkg.Name -> V.Version -> FilePath
package (PackageCache dir) name version =
  dir </> Pkg.toFilePath name </> V.toChars version



-- CACHE


getReplCache :: IO FilePath
getReplCache =
  getCacheDir "repl"


getCacheDir :: FilePath -> IO FilePath
getCacheDir projectName =
  do  home <- getElmHome
      let root = home </> compilerVersion </> projectName
      Dir.createDirectoryIfMissing True root
      return root


getElmHome :: IO FilePath
getElmHome =
  do  maybeCustomHome <- Env.lookupEnv "ELM_HOME"
      case maybeCustomHome of
        Just customHome -> return customHome
        Nothing -> Dir.getAppUserDataDirectory "elm"
