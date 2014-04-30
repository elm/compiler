module Build.Print where

import System.IO   (hPutStrLn, stderr)
import System.Exit (exitFailure)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified AST.Module as M
import qualified AST.PrettyPrint as Pretty
import qualified Type.Alias as Alias
import qualified Text.PrettyPrint as P

moduleTypes :: Map.Map String M.Interface -> M.CanonicalModule -> IO ()
moduleTypes interfaces modul =
    types interfaces (M.types body) (M.aliases body) (M.imports modul)
    where
      body = M.body modul

interfaceTypes :: Map.Map String M.Interface -> M.Interface -> IO ()
interfaceTypes interfaces iface =
    types interfaces (M.iTypes iface) (M.iAliases iface) (M.iImports iface)

types interfaces types' aliases imports =
    do putStrLn ""
       mapM_ printType (Map.toList types')
       putStrLn ""
    where
      rules = Alias.rules interfaces aliases imports

      printType (n,t) = do
        print $ P.hsep [ Pretty.variable n
                       , P.text ":"
                       , Pretty.pretty (Alias.realias rules t) ]

errors :: [P.Doc] -> IO ()
errors errs =
  mapM_ print (List.intersperse (P.text " ") errs)

failure :: String -> IO a
failure msg = hPutStrLn stderr msg >> exitFailure
