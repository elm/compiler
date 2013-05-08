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

-- Part 1
-- ------
-- Add a build callout
-- We need to build elm-doc and run it because that generates the file "docs.json" needs by Libraries.hs
--   which is part of the elm library and executable
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
rtsDir lbi = (buildDir lbi) </> ".." </> "data"      -- git won't look in dist + cabal will clean it
jsDir lbi = (buildDir lbi) </> ".." </> "js"

-- The runtime is called:
rts lbi = (rtsDir lbi) </> "elm-runtime.js"

-- The json file is called:

-- The elm-docs executable is called:
elmDoc = "elm-doc"
elm_doc lbi = (buildDir lbi) </> elmDoc </> elmDoc

types lbi = (rtsDir lbi) </> "docs.json"

-- buildDir with LocalBuildInfo points to "dist/build" (usually)
elm lbi = (buildDir lbi) </> "elm" </> "elm"

-- Care!  This appears to be based on an unstable API
-- See: http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html#2


main :: IO ()
main = defaultMainWithHooks simpleUserHooks { {-- buildHook = myBuild, --} postBuild = myPostBuildWithTypes }


-- Build

-- Not currently used.  buildTypes is in 'myPostBuildWithTypes'
-- If using this again, change postBuild to 'myPostBuild'
-- Purpose is to make sure docs.json was built before elm exec was (as elm exec depended on it)
-- This is no longer true and the code below seems to affect cabal's build dependencies
myBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuild pd lbi uh bf = do
    putStrLn $ "Custom build step started: compile " ++ elmDoc
    withExe pd (\x -> putStrLn (exeName x))
    buildHook simpleUserHooks (filterExe elmDoc pd) (filterLBI elmDoc lbi) uh bf
    putStrLn "Custom build step started: build docs.json"
    buildTypes lbi      -- see note(1) below
    putStrLn "Custom build step started: compile everything"
    buildHook simpleUserHooks pd lbi uh bf

-- note(1): We use to include docs.json directly into LoadLibraries at compile time
-- If docs.json is used in other (template) haskell files, they should be copied
-- and compiled in a separate directory (eg, dist/copiedSrc).
-- This is to make sure they are re-compiled on docs.json changes.
-- Copying is a better solution than 'touch'ing the source files
-- (touch is non-portable and confusing wrt RCS).

-- In the PackageDescription, the list of stuff to build is held in library (in a Maybe)
-- and the executables list.  We want a PackageDescription that only mentions the executable 'name'
filterExe name pd = pd {
    library = Nothing,
    executables = filter (\x -> (exeName x == name)) (executables pd)
    }

-- It's not enough to fix the PackageDescription, we also have to fix the LocalBuildInfo.
-- This includes the component build order (data ComponentName) which is horribly internal.
filterLBI name lbi = lbi {
    libraryConfig = Nothing,
    compBuildOrder = [CExeName name],
    executableConfigs = filter (\a -> (fst a == name)) (executableConfigs lbi)
    }

buildTypes lbi = do
  createDirectoryIfMissing False (rtsDir lbi)     -- dist should already exist
  files <- getFiles ".elm" "libraries"
  system (elm_doc lbi ++ " " ++ unwords files ++ " > " ++ (types lbi))
  putStrLn $ "Custom build step completed: " ++ elmDoc


-- Post Build

myPostBuildWithTypes :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuildWithTypes as bfs pd lbi = do
    putStrLn "Custom build step started: build docs.json"
    buildTypes lbi      -- see note(1) below
    myPostBuild as bfs pd lbi

myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild as bfs pd lbi = do
    putStrLn "Custom post build step started: build elm-runtime.js"
    buildRuntime lbi
    postBuild simpleUserHooks as bfs pd lbi

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

appendElm lbi file = do
  jsFile <- runElm lbi file
  appendJS lbi jsFile

-- replace 'system' call with 'runProcess' which handles args better and allows env variable
-- "Elm_datadir" which is used by LoadLibraries to find docs.json
runElm :: LocalBuildInfo -> String -> IO FilePath
runElm lbi file = do
  rts_c <- canonicalizePath (rts lbi)       -- dist/data/elm-runtime.js
  let js = jsDir lbi        -- dist/js
  let j = dropFileName (js </> file)        -- dist/js/libraries/
  createDirectoryIfMissing True j       -- must do before any canonicalization
  out_c <- canonicalizePath js      -- dist/js (root folder)
  elm_c <- canonicalizePath (elm lbi)       -- dist/build/elm/elm
  rtd_c <- canonicalizePath (rtsDir lbi)        -- dist/data (for docs.json)
  handle <- runProcess elm_c ["--only-js", "--no-prelude", "--output-directory="++out_c, file]
            Nothing (Just [("Elm_datadir", rtd_c)]) Nothing Nothing Nothing
  exitCode <- waitForProcess handle
  return $ j </> replaceExtension (takeFileName file) ".js"


buildRuntime lbi = do
  createDirectoryIfMissing False (rtsDir lbi)     -- dist should already exist
  writeFile (rts lbi) "var window = window || {}; Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};\n\
                \Elm.Graphics = {}; ElmRuntime = {}; ElmRuntime.Render = {}\n"
  mapM_ (appendJS lbi)  =<< getFiles ".js" "libraries"
  mapM_ (appendElm lbi) =<< getFiles ".elm" "libraries"
  mapM_ (appendJS lbi)  =<< getFiles ".js"  "runtime"
