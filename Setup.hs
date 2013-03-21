import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.PackageDescription

import System.Cmd
import System.Directory
import System.FilePath
import System.IO

-- Add a post-build callout.
-- We need to build the runtime.js after we've built elm,
-- but before cabal does the install file copy step

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postBuild = myPostBuild }

-- Care!  This appears to be based on an unstable API
-- See: http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html#2

myPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostBuild as bfs pd lbi = do
    putStrLn "Custom build step started"
    buildRuntime lbi
    buildTypes lbi
    postBuild simpleUserHooks as bfs pd lbi

-- put js file in dist folder so git doesn't notice
rtsDir = "dist" </> "data"

rts = rtsDir </> "elm-runtime.js"
types = rtsDir </> "docs.json"

-- buildDir with LocalBuildInfo points to "dist/build" (usually)
elm lbi = (buildDir lbi) </> "elm" </> "elm"
elm_doc lbi = (buildDir lbi) </> "elm-doc" </> "elm-doc"


getFiles ext dir = do
  contents <- map (dir </>) `fmap` getDirectoryContents dir
  let files = filter (\f -> takeExtension f == ext) contents
      dirs  = filter (not . hasExtension) contents
  filess <- mapM (getFiles ext) dirs
  return (files ++ concat filess)

appendJS file = do
  putStrLn (dropExtension file)
  str <- readFile file
  length str `seq` return ()
  appendFile rts str

appendElm lbi file = do
  system (elm lbi ++ " --only-js " ++ file)
  let jsFile = replaceExtension file ".js"
  appendJS jsFile
  removeFile jsFile

buildRuntime lbi = do
  createDirectoryIfMissing False rtsDir     -- dist should already exist
  writeFile rts "Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};\n\
                \Elm.Graphics = {}; ElmRuntime = {}; ElmRuntime.Render = {}\n"
  mapM_ appendJS =<< getFiles ".js" "libraries"
  mapM_ (appendElm lbi) =<< getFiles ".elm" "libraries"
  mapM_ appendJS  =<< getFiles ".js"  "runtime"
  putStrLn "\n+------------------------------------------+\
           \\n|  Success building runtime and libraries! |\
           \\n+------------------------------------------+\n"

buildTypes lbi = do
  files <- getFiles ".elm" "libraries"
  system (elm_doc lbi ++ " " ++ unwords files ++ " > " ++ types)
  putStrLn "Custom build step completed: elm-doc"
