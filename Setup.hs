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

-- The interfaces for the Standard Libraries live in:
interfaces :: LocalBuildInfo -> FilePath
interfaces lbi = rtsDir lbi </> "interfaces.data"

-- buildDir with LocalBuildInfo points to "dist/build" (usually)
elm lbi = buildDir lbi </> "elm" </> "elm"

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

-- It's not enough to fix the PackageDescription, we also have to fix the
-- LocalBuildInfo. This includes the component build order (data ComponentName)
-- which is horribly internal.
filterLBI :: String -> LocalBuildInfo -> LocalBuildInfo
filterLBI name lbi = lbi {
    libraryConfig = Nothing,
    compBuildOrder = [CExeName name],
    executableConfigs = filter (\a -> (fst a == name)) (executableConfigs lbi)
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
    removeDirectoryRecursive (tempDir lbi)
    postBuild simpleUserHooks as bfs pd lbi


compileLibraries lbi = do
  let temp = tempDir lbi                    -- temp
      rts  = rtsDir  lbi                    -- data
  createDirectoryIfMissing True temp
  createDirectoryIfMissing True rts
  out_c <- canonicalizePath temp            -- temp (root folder)
  elm_c <- canonicalizePath (elm lbi)       -- dist/build/elm/elm
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
  print =<< getCurrentDirectory
  files <- getFiles ".elm" "."
  files <- unzip `fmap` mapM make files
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
  writeFile (rts lbi) "Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};\n\
                      \Elm.Graphics = {}; ElmRuntime = {}; ElmRuntime.Render = {};\n"
  mapM_ (appendJS lbi) =<< getFiles ".js" "libraries"
  mapM_ (appendJS lbi) elmos
  mapM_ (appendJS lbi) =<< getFiles ".js" "runtime"

getFiles ext dir = do
  contents <- map (dir </>) `fmap` getDirectoryContents dir
  let files = filter (\f -> takeExtension f == ext) contents
      dirs  = filter (not . hasExtension) contents
  filess <- mapM (getFiles ext) dirs
  return (files ++ concat filess)

appendJS lbi file = do
  putStrLn (dropExtension file)
  str <- readFile file
  length str `seq` return ()
  appendFile (rts lbi) str
