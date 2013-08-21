module Type.Inference where

import qualified Data.Map as Map

import qualified Type.Type as T
import qualified Type.Environment as Env
import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Constrain.Declaration as TcDecl
import qualified Type.Solve as Solve

import SourceSyntax.Module as Module
import qualified SourceSyntax.Expression as Expr
import SourceSyntax.Location (Located, noneNoDocs)
import SourceSyntax.PrettyPrint
import SourceSyntax.Type (Type)
import Text.PrettyPrint
import qualified Type.State as TS
import Type.ExtraChecks (extraChecks)
import Control.Monad.State
import Control.Arrow (second)
import Transform.SortDefinitions as Sort
import qualified Data.Traversable as Traverse

import System.IO.Unsafe  -- Possible to switch over to the ST monad instead of
                         -- the IO monad. I don't think that'd be worthwhile.


infer :: Interfaces -> MetadataModule t v -> Either [Doc] (Map.Map String Type)
infer interfaces modul = unsafePerformIO $ do
  env <- Env.initialEnvironment
             (datatypes modul ++ concatMap iAdts (Map.elems interfaces))
             (aliases modul ++ concatMap iAliases (Map.elems interfaces))
  ctors <- forM (Map.keys (Env.constructor env)) $ \name ->
               do (_, vars, args, result) <- Env.freshDataScheme env name
                  return (name, (vars, foldr (T.==>) result args))

  importedVars <-
      forM (concatMap (Map.toList . iTypes) $ Map.elems interfaces) $ \(name,tipe) ->
          (,) name `fmap` Env.instantiateType env tipe Map.empty

  let allTypes = ctors ++ importedVars
      vars = concatMap (fst . snd) allTypes
      header = Map.map snd (Map.fromList allTypes)
      environ = noneNoDocs . T.CLet [ T.Scheme vars [] (noneNoDocs T.CTrue) header ]

  fvar <- T.var T.Flexible
  constraint <- environ `fmap` TcExpr.constrain env (program modul) (T.VarN fvar)

  state <- execStateT (Solve.solve constraint) TS.initialState
  case TS.sErrors state of
    errors@(_:_) -> Left `fmap` sequence (reverse errors)
    [] -> let types = Map.difference (TS.sSavedEnv state) header in
          do srcTypes <- Traverse.traverse T.toSrcType types
             return $ Right srcTypes

