{-# LANGUAGE CPP #-}

module LoadLibraries (docs) where

import Language.Haskell.TH.Syntax
import Paths_Elm
import System.Directory
import System.FilePath

docs :: Q Exp
docs = liftString =<< qRunIO (readDocs dataDir)

-- When compiling elm (library and executable), docs.json isn't (yet) in the standard location
-- Therefore, Elm.cabal overrides the location.
-- Since this is only used by the TemplateHaskell, which reads the file at compile time,
-- this should be ok.

dataDir :: IO FilePath
#if defined ELM_COMPILEDATADIR
dataDir = canonicalizePath (ELM_COMPILEDATADIR </> "docs.json")
#else
dataDir = getDataFileName "docs.json"
#endif

readDocs :: IO FilePath -> IO String
readDocs path = do
  name <- path
  exists <- doesFileExist name
  if exists then readFile name
            else putStrLn warning >> return "{\"modules\":[]}"

warning = "Warning! Types for standard library not loaded properly!\n\
          Run the following commands after compilation:\n\
          \n\
              touch compiler/Model/LoadLibraries.hs\n\
              cabal install\n\n"
