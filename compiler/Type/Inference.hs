module Type.Inference where

import qualified Data.Map as Map

import qualified Type.Type as T
import qualified Type.Environment as Env
import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Constrain.Declaration as TcDecl
import qualified Type.Solve as Solve

import SourceSyntax.Module as Module
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
  let insert (name,method) dict = Map.insertWith (++) name [method] dict
      localImports = foldr insert Map.empty (imports modul)
      interfaces = Map.intersectionWithKey Module.canonicalize localImports interfaces'

--  mapM print (concatMap iAdts (Map.elems interfaces))

  env <- Env.initialEnvironment (datatypes modul ++ concatMap iAdts (Map.elems interfaces))
  ctors <- forM (Map.keys (Env.constructor env)) $ \name ->
               do (_, vars, args, result) <- Env.freshDataScheme env name
                  return (name, (vars, foldr (T.==>) result args))

--  mapM print (map Map.toList $ Map.elems locals)
--  mapM print (imports modul)

  importedVars <-
      forM (concatMap (Map.toList . iTypes) $ Map.elems interfaces) $ \(name,tipe) ->
          (,) name `fmap` Env.instantiateType env tipe Map.empty

  let allTypes = ctors ++ importedVars
      vars = concatMap (fst . snd) allTypes
      header = Map.map snd (Map.fromList allTypes)
      environ = T.CLet [ T.Scheme vars [] T.CTrue header ]

  fvar <- T.var T.Flexible
  constraint <- environ `fmap` TcExpr.constrain env (program modul) (T.VarN fvar)
  --print =<< T.extraPretty constraint

  state <- execStateT (Solve.solve constraint) TS.initialState
  let errors = TS.sErrors state
  if null errors
      then return $ Right (Map.difference (TS.sSavedEnv state) header)
      else Left `fmap` sequence errors

