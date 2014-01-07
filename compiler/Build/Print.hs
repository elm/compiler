module Build.Print where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified SourceSyntax.Module as M
import qualified SourceSyntax.PrettyPrint as Pretty
import qualified Type.Alias as Alias
import qualified Text.PrettyPrint as P

metaTypes :: Map.Map String M.ModuleInterface -> M.MetadataModule -> IO ()
metaTypes interfaces meta =
    types interfaces (M.types meta) (M.aliases meta) (M.imports meta)

interfaceTypes :: Map.Map String M.ModuleInterface -> M.ModuleInterface -> IO ()
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