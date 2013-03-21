
module LoadLibraries (docs) where

import Language.Haskell.TH.Syntax
import Paths_Elm
import System.Directory

docs :: Q Exp
docs = liftString =<< qRunIO readDocs

readDocs :: IO String
readDocs = do
  name <- getDataFileName "docs.json"
  exists <- doesFileExist name
  if exists then readFile name
            else putStrLn warning >> return "{\"modules\":[]}"

warning = "Warning! Types for standard library not loaded properly!\n\
          \Run the following commands after compilation:\n\
          \\n\
          \    touch compiler/Model/LoadLibraries.hs\n\
          \    cabal install\n\n"