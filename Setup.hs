import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription

import System.Cmd
import System.Directory
import System.FilePath
import System.IO
import System.Process

import Control.Monad
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List

-- Part 1
-- ------
-- Add a build callout
-- We need to build elm-doc and run it because that generates the file "docs.json"
-- needs by Libraries.hs which is part of the elm library and executable
-- Unfort. there seems to be no way to tell cabal that:
--   (a) elm-doc generates docs.json, and
--   (b) elm (library) depends on docs.json
-- Therefore, we either use make instead (or a script), or hack around in cabal

-- Part 2
-- ------
-- Add a post-build callout.
-- We need to build the runtime.js after we've built elm (because we use elm to generate some of the JavaScript),
-- but before cabal does the install file copy step

-- Assumptions
-- Elm.cabal expects the generated files to end up in dist/data
-- git won't look in dist + cabal will clean it
rtsDir :: LocalBuildInfo -> FilePath
rtsDir lbi = "data"

tempDir :: LocalBuildInfo -> FilePath
tempDir lbi = "temp"

-- The runtime is called:
rts :: LocalBuildInfo -> FilePath
rts lbi = rtsDir lbi </> "elm-runtime.js"

-- The runtime is called:
docs :: LocalBuildInfo -> FilePath
docs lbi = rtsDir lbi </> "docs.json"

-- The interfaces for the Standard Libraries live in:
interfaces :: LocalBuildInfo -> FilePath
interfaces lbi = rtsDir lbi </> "interfaces.data"

elm :: LocalBuildInfo -> FilePath
elm lbi = buildDir lbi </> "elm" </> "elm"

document :: LocalBuildInfo -> FilePath
document lbi = buildDir lbi </> "elm-doc" </> "elm-doc"

-- Care!  This appears to be based on an unstable API
-- See: http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html#2


main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postBuild = myPostBuild }


-- Build

-- note(1): We use to include docs.json directly into LoadLibraries at compile time
-- If docs.json is used in other (template) haskell files, they should be copied
-- and compiled in a separate directory (eg, dist/copiedSrc).
-- This is to make sure they are re-compiled on docs.json changes.
-- Copying is a better solution than 'touch'ing the source files
-- (touch is non-portable and confusing wrt RCS).

-- In the PackageDescription, the list of stuff to build is held in library
-- (in a Maybe) and the executables list.  We want a PackageDescription that
-- only mentions the executable 'name'
filterExe :: String -> PackageDescription -> PackageDescription
filterExe name pd = pd {
    library = Nothing,
    executables = filter (\x -> (exeName x == name)) (executables pd)
    }


-- Post Build

myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild as bfs pd lbi = do
    putStrLn "Custom build step: compiling standard libraries"
    (elmos, elmis) <- compileLibraries lbi
    putStrLn "Custom build step: build interfaces.data"
    buildInterfaces lbi elmis
    putStrLn "Custom build step: build elm-runtime.js"
    buildRuntime lbi elmos
    putStrLn "Custom build step: build docs.json"
    buildDocs lbi
    removeDirectoryRecursive (tempDir lbi)
    removeDirectoryRecursive ("libraries" </> "docs")
    postBuild simpleUserHooks as bfs pd lbi


compileLibraries lbi = do
  let temp = tempDir lbi                    -- temp
      rts  = rtsDir  lbi                    -- data
  createDirectoryIfMissing True temp
  createDirectoryIfMissing True rts
  out_c <- canonicalizePath temp            -- temp (root folder)
  elm_c <- canonicalizePath (elm lbi)       -- dist/build/elm/elm
  doc_c <- canonicalizePath (document lbi)  -- dist/build/elm-doc/elm-doc
  rtd_c <- canonicalizePath rts             -- data

  let make file = do
        -- replace 'system' call with 'runProcess' which handles args better
        -- and allows env variable "Elm_datadir" which is used by LoadLibraries
        -- to find docs.json
        let args = [ "--only-js", "--make", "--no-prelude"
                   , "--cache-dir="++out_c, "--build-dir="++out_c, file ]
            arg = Just [("Elm_datadir", rtd_c)]
        handle <- runProcess elm_c args Nothing arg Nothing Nothing Nothing
        exitCode <- waitForProcess handle
        return ( out_c </> replaceExtension file "elmo"
               , out_c </> replaceExtension file "elmi")

  setCurrentDirectory "libraries"
  paths <- getFiles ".elm" "."
  files <- unzip `fmap` mapM make paths
  mapM_ (\path -> rawSystem doc_c [path]) paths
  setCurrentDirectory ".."
  return files

buildInterfaces :: LocalBuildInfo -> [FilePath] -> IO ()
buildInterfaces lbi elmis = do
  createDirectoryIfMissing True (rtsDir lbi)
  let ifaces = interfaces lbi
  ifaceHandle <- openBinaryFile ifaces WriteMode
  BS.hPut ifaceHandle (Binary.encode (length elmis))
  let append file = do
        handle <- openBinaryFile file ReadMode
        bits <- hGetContents handle
        length bits `seq` hPutStr ifaceHandle bits
        hClose handle
  mapM_ append elmis
  hClose ifaceHandle

buildRuntime :: LocalBuildInfo -> [FilePath] -> IO ()
buildRuntime lbi elmos = do
  createDirectoryIfMissing True (rtsDir lbi)
  let rts' = rts lbi
  writeFile rts' "var Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};\n\
                 \var ElmRuntime = {}; ElmRuntime.Render = {};\n"
  mapM_ (appendTo rts') =<< getFiles ".js" "libraries"
  mapM_ (appendTo rts') elmos
  mapM_ (appendTo rts') =<< getFiles ".js" "runtime"

buildDocs :: LocalBuildInfo -> IO ()
buildDocs lbi = do
  createDirectoryIfMissing True (rtsDir lbi)
  let docs' = docs lbi
  writeFile docs' "[\n"
  json <- getFiles ".json" ("libraries" </> "docs")
  let appends = map (appendTo docs') json
      addCommas = List.intersperse (appendFile docs' ",\n")
  sequence_ (addCommas appends)
  appendFile docs' "\n]"

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
