-- This file compiles all library and runtime code 
-- into compiler/elm-runtime.js. It then rebuilds the compiler.
-- Run it with the command "runHaskell Build.hs".

import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Language.Elm

rts = "compiler" </> "elm-runtime.js"
types = "compiler" </> "types.json"

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

appendElm file = do
  system ("elm --only-js " ++ file)
  let jsFile = replaceExtension file ".js"
  appendJS jsFile
  removeFile jsFile

main = do
  writeFile rts "Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};\n\
                \Elm.Graphics = {}; ElmRuntime = {}; ElmRuntime.Render = {}\n"
  mapM_ appendJS  =<< getFiles ".js"  "libraries"
  files <- getFiles ".elm" "libraries"
  mapM_ appendElm files
  mapM_ appendJS  =<< getFiles ".js"  "runtime"
  putStrLn "\n+------------------------------------------+\
           \\n|  Success building runtime and libraries! |\
           \\n+------------------------------------------+\n"
  system ("elm-doc " ++ unwords files ++ " > " ++ types)
  system ("cabal install compiler" </> "Elm.cabal")
  exitSuccess