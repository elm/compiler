-- This is a dummy file, but it should not be removed.
-- It makes it possible to build the compiler in GHCi, so you
-- can do some testing from within emacs (or whatever inferior
-- editor you use instead).

module Paths_Elm where

import Data.Version
import System.FilePath

getDataFileName :: FilePath -> IO FilePath
getDataFileName path = return ("dist" </> "data" </> path)

version :: Version
version = Version [0,8,0,3] []