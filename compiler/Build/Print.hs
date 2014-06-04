module Build.Print where

import System.IO   (hPutStrLn, stderr)
import System.Exit (exitFailure)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified AST.Module as Module
import qualified AST.PrettyPrint as Pretty
import qualified Text.PrettyPrint as P

types :: Module.Types -> IO ()
types ts =
    do putStrLn ""
       mapM_ printType (Map.toList ts)
       putStrLn ""
    where
      printType (n,t) = do
        print $ P.hsep [ Pretty.variable n
                       , P.text ":"
                       , Pretty.pretty t ]

errors :: [P.Doc] -> IO ()
errors errs =
  mapM_ print (List.intersperse (P.text " ") errs)

failure :: String -> IO a
failure msg = hPutStrLn stderr msg >> exitFailure
