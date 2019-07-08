{-# OPTIONS_GHC -Wall #-}
module Stuff
  ( details
  , interfaces
  , objects
  , prepublishDir
  , elmi
  , elmo
  , temp
  , findRoot
  , withRootLock
  , withRegistryLock
  , PackageCache
  , getPackageCache
  , registry
  , package
  , getReplCache
  , getElmHome
  )
  where


import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FileLock as Lock
import qualified System.FilePath as FP
import System.FilePath ((</>), (<.>))

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.Version as V



-- PATHS


stuff :: FilePath -> FilePath
stuff root =
  root </> "elm-stuff" </> compilerVersion


details :: FilePath -> FilePath
details root =
  stuff root </> "d.dat"


interfaces :: FilePath -> FilePath
interfaces root =
  stuff root </> "i.dat"


objects :: FilePath -> FilePath
objects root =
  stuff root </> "o.dat"


prepublishDir :: FilePath -> FilePath
prepublishDir root =
  stuff root </> "prepublish"


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
  stuff root </> ModuleName.toHyphenPath name <.> ext



-- TEMP


temp :: FilePath -> String -> FilePath
temp root ext =
  stuff root </> "temp" <.> ext



-- ROOT


findRoot :: IO (Maybe FilePath)
findRoot =
  do  dir <- Dir.getCurrentDirectory
      findRootHelp (FP.splitDirectories dir)


findRootHelp :: [String] -> IO (Maybe FilePath)
findRootHelp dirs =
  case dirs of
    [] ->
      return Nothing

    _:_ ->
      do  exists <- Dir.doesFileExist (FP.joinPath dirs </> "elm.json")
          if exists
            then return (Just (FP.joinPath dirs))
            else findRootHelp (init dirs)



-- LOCKS


withRootLock :: FilePath -> IO a -> IO a
withRootLock root work =
  do  let dir = stuff root
      Dir.createDirectoryIfMissing True dir
      Lock.withFileLock (dir </> "lock") Lock.Exclusive (\_ -> work)


withRegistryLock :: PackageCache -> IO a -> IO a
withRegistryLock (PackageCache dir) work =
  Lock.withFileLock (dir </> "lock") Lock.Exclusive (\_ -> work)



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
