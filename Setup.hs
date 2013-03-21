import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription

import System.Cmd
import System.Directory
import System.FilePath
import System.IO

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

-- The runtime is called:
rts lbi = (rtsDir lbi) </> "elm-runtime.js"

-- The json file is called:

-- The elm-docs executable is called:
elmDoc = "elm-doc"
elm_doc lbi = (buildDir lbi) </> elmDoc </> elmDoc

types lbi = (rtsDir lbi) </> "docs.json"

-- buildDir with LocalBuildInfo points to "dist/build" (usually)
elm lbi = (buildDir lbi) </> "elm" </> "elm"

-- "touch" is available as a system command.
-- There is no portable file touch (unix-compat might work but leads to cabal library dependency
--    issues: see: http://hackage.haskell.org/trac/summer-of-code/ticket/1602 )
-- Files that need recompilation after docs.json is updated
--    (Would be nice if this list was in the cabal file, but can't see any way to do that)
docTouches = ["compiler" </> "Model" </> "LoadLibraries.hs"]

-- Care!  This appears to be based on an unstable API
-- See: http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html#2


main :: IO ()
main = defaultMainWithHooks simpleUserHooks { buildHook = myBuild, postBuild = myPostBuild }


-- Build

myBuild :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuild pd lbi uh bf = do
    putStrLn $ "Custom build step started: compile " ++ elmDoc
    withExe pd (\x -> putStrLn (exeName x))
    buildHook simpleUserHooks (filterExe elmDoc pd) (filterLBI elmDoc lbi) uh bf
    putStrLn "Custom build step started: build docs.json"
    buildTypes lbi
    forM_ docTouches (\s -> system ("touch " ++ s))     -- NB non-portable
    putStrLn "Custom build step started: compile everything"
    buildHook simpleUserHooks pd lbi uh bf

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
  system (elm lbi ++ " --only-js " ++ file)
  let jsFile = replaceExtension file ".js"
  appendJS lbi jsFile
  removeFile jsFile

buildRuntime lbi = do
  createDirectoryIfMissing False (rtsDir lbi)     -- dist should already exist
  writeFile (rts lbi) "Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};\n\
                \Elm.Graphics = {}; ElmRuntime = {}; ElmRuntime.Render = {}\n"
  mapM_ (appendJS lbi)  =<< getFiles ".js" "libraries"
  mapM_ (appendElm lbi) =<< getFiles ".elm" "libraries"
  mapM_ (appendJS lbi)  =<< getFiles ".js"  "runtime"
  putStrLn "\n+------------------------------------------+\
           \\n|  Success building runtime and libraries! |\
           \\n+------------------------------------------+\n"
