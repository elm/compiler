
module LoadLibraries where

import Language.Haskell.TH.Syntax
import Paths_Elm

docs :: Q Exp
docs = liftString =<< qRunIO (do
                               s <- readFile =<< getDataFileName "docs.json"
                               putStrLn s
                               return s
                             )

