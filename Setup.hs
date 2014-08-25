{-# OPTIONS_GHC -W #-}
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription

import System.Directory
import System.FilePath
import System.IO
import System.Process

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List

-- OVERVIEW
-- --------
-- Add a post-build callout that compiles the standard libraries with the new
-- compiler before cabal copies static files to actually install things.

assetsDir :: FilePath
assetsDir = "data"

tempDir :: FilePath
tempDir = "temp"

-- The runtime is called:
runtime :: FilePath
runtime = assetsDir </> "elm-runtime.js"

-- The runtime is called:
docs :: FilePath
docs = assetsDir </> "docs.json"

-- The interfaces for the Standard Libraries live in:
interfaces :: FilePath
interfaces = assetsDir </> "interfaces.data"

elm :: LocalBuildInfo -> FilePath
elm lbi = buildDir lbi </> "elm" </> "elm"

document :: LocalBuildInfo -> FilePath
document lbi = buildDir lbi </> "elm-doc" </> "elm-doc"

-- Care!  This appears to be based on an unstable API
-- See: http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html#2


main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postBuild = myPostBuild }



-- Post Build

myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild args flags packageDescription lbi = do
    putStrLn "Custom build step: compiling standard libraries"
    (elmos, elmis) <- compileLibraries lbi
    putStrLn "Custom build step: build interfaces.data"
    buildInterfaces elmis
    putStrLn "Custom build step: build elm-runtime.js"
    buildRuntime elmos
    putStrLn "Custom build step: build docs.json"
    buildDocs
    removeDirectoryRecursive tempDir
    removeDirectoryRecursive ("libraries" </> "docs")
    postBuild simpleUserHooks args flags packageDescription lbi


compileLibraries lbi = do
  createDirectoryIfMissing True tempDir
  createDirectoryIfMissing True assetsDir
  out_c <- canonicalizePath tempDir         -- temp (root folder)
  elm_c <- canonicalizePath (elm lbi)       -- dist/build/elm/elm
  doc_c <- canonicalizePath (document lbi)  -- dist/build/elm-doc/elm-doc
  rtd_c <- canonicalizePath assetsDir       -- data

  let make file = do
        let args =
              [ "--only-js"
              , "--make"
              , "--no-prelude"
              , "--cache-dir=" ++ out_c
              , "--build-dir=" ++ out_c
              , file
              ]
        let arg = Just [("Elm_datadir", rtd_c)]
        handle <- runProcess elm_c args Nothing arg Nothing Nothing Nothing
        _exitCode <- waitForProcess handle
        return ( out_c </> replaceExtension file "elmo"
               , out_c </> replaceExtension file "elmi"
               )

  setCurrentDirectory "libraries"
  paths <- getFiles ".elm" "."
  files <- unzip `fmap` mapM make paths
  mapM_ (\path -> rawSystem doc_c [path]) paths
  setCurrentDirectory ".."
  return files

buildInterfaces :: [FilePath] -> IO ()
buildInterfaces elmis = do
  createDirectoryIfMissing True assetsDir
  ifaceHandle <- openBinaryFile interfaces WriteMode
  BS.hPut ifaceHandle (Binary.encode (length elmis))
  let append file = do
        handle <- openBinaryFile file ReadMode
        bits <- hGetContents handle
        length bits `seq` hPutStr ifaceHandle bits
        hClose handle
  mapM_ append elmis
  hClose ifaceHandle

buildRuntime :: [FilePath] -> IO ()
buildRuntime elmos = do
  createDirectoryIfMissing True assetsDir
  writeFile runtime
      "'use strict';\n\
      \var Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};\n\
      \var ElmRuntime = {}; ElmRuntime.Render = {};\n"
  mapM_ (appendTo runtime) =<< getFiles ".js" "libraries"
  mapM_ (appendTo runtime) elmos
  mapM_ (appendTo runtime) =<< getFiles ".js" "runtime"

buildDocs :: IO ()
buildDocs = do
  createDirectoryIfMissing True assetsDir
  writeFile docs "[\n"
  json <- getFiles ".json" ("libraries" </> "docs")
  let appends = map (appendTo docs) json
      addCommas = List.intersperse (appendFile docs ",\n")
  sequence_ (addCommas appends)
  appendFile docs "\n]"

getFiles ext dir = do
  contents <- map (dir </>) `fmap` getDirectoryContents dir
  let files = filter (\f -> takeExtension f == ext) contents
      dirs  = filter (not . hasExtension) contents
  filess <- mapM (getFiles ext) dirs
  return (files ++ concat filess)

appendTo :: FilePath -> FilePath -> IO ()
appendTo destination source = do
  str <- readFile source
  length str `seq` return ()
  appendFile destination str
