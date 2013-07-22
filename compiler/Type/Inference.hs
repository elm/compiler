module Type.Inference where

import qualified Data.Map as Map

import qualified Type.Type as T
import qualified Type.Environment as Env
import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Constrain.Declaration as TcDecl
import qualified Type.Solve as Solve

import SourceSyntax.Module
import qualified SourceSyntax.Expression as Expr
import SourceSyntax.PrettyPrint
import Text.PrettyPrint
import qualified Type.State as TS
import Control.Monad.State
import Control.Arrow (second)
import Transform.SortDefinitions as Sort

import System.IO.Unsafe  -- Possible to switch over to the ST monad instead of
                         -- the IO monad. Not sure if that'd be worthwhile.


infer :: Interfaces -> MetadataModule t v -> Either [Doc] (Map.Map String T.Variable)
infer interfaces' modul = unsafePerformIO $ do
  let localImports = Map.fromList (imports modul)
      interfaces = Map.intersectionWithKey canonicalize localImports interfaces'

--  mapM print (concatMap iAdts (Map.elems interfaces))

  env <- Env.initialEnvironment (datatypes modul ++ concatMap iAdts (Map.elems interfaces))
  var <- T.flexibleVar
  ctors <- forM (Map.keys (Env.constructor env)) $ \name ->
               do (_, vars, tipe) <- Env.freshDataScheme env name
                  return (name, (vars, tipe))

  let locals = Map.intersectionWithKey combine localImports interfaces
      combine name importMethod interface =
          let tipes = iTypes interface in
          case importMethod of
            As alias -> Map.mapKeys (\v -> alias ++ "." ++ v) tipes
            Hiding hidens -> foldr Map.delete tipes hidens
            Importing visibles ->
                Map.intersection tipes (Map.fromList [ (v,()) | v <- visibles ])

--  mapM print (map Map.toList $ Map.elems locals)
--  mapM print (imports modul)

  importedVars <-
      forM (concatMap Map.toList $ Map.elems locals) $ \(name,tipe) ->
          (,) name `fmap` Env.instantiateTypeWithContext env tipe Map.empty

  let allTypes = ctors ++ importedVars
      vars = concatMap (fst . snd) allTypes
      header = Map.map snd (Map.fromList allTypes)
      environ = T.CLet [ T.Scheme vars [] T.CTrue header ]
  constraint <- environ `fmap` TcExpr.constrain env (program modul) (T.VarN var)
--  print =<< T.extraPretty constraint
  state <- execStateT (Solve.solve constraint) TS.initialState
  let errors = TS.sErrors state
  if null errors
      then return $ Right (Map.difference (TS.sSavedEnv state) header)
      else Left `fmap` sequence errors

