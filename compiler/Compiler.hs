module Main where

import System.Console.CmdArgs
import GHC.Conc
import Build.Flags
import Build.Build

main :: IO ()
main = do setNumCapabilities =<< getNumProcessors
          compileArgs =<< cmdArgs flags

compileArgs :: Flags -> IO ()
compileArgs flags =
    case files flags of
      [] -> putStrLn "Usage: elm [OPTIONS] [FILES]\nFor more help: elm --help"
      fs -> mapM_ (build flags) fs
