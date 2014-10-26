module Type.Inference where

import qualified Data.Map as Map

import qualified Type.Type as T
import qualified Type.Environment as Env
import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Solve as Solve

import qualified AST.Annotation as A
import AST.Module as Module
import AST.Type (CanonicalType)
import qualified AST.Variable as Var
import Text.PrettyPrint
import qualified Type.State as TS
import qualified Type.ExtraChecks as Check
import Control.Arrow (first, second)
import Control.Monad.State (execStateT, forM)
import Control.Monad.Error (ErrorT, runErrorT, liftIO, throwError)

import System.IO.Unsafe  -- Possible to switch over to the ST monad instead of
                         -- the IO monad. I don't think that'd be worthwhile.


infer :: Interfaces -> CanonicalModule -> Either [Doc] (Map.Map String CanonicalType)
infer interfaces modul =
  unsafePerformIO $ runErrorT $
    do  (header, constraint) <- genConstraints interfaces modul

        state <- liftIO $ execStateT (Solve.solve constraint) TS.initialState

        () <- case TS.sErrors state of
                errors@(_:_) -> throwError errors
                []           -> return ()

        () <- Check.portTypes (program (body modul))

        let header' = Map.delete "::" header
            types = Map.difference (TS.sSavedEnv state) header'

        Check.mainType types


genConstraints
    :: Interfaces
    -> CanonicalModule
    -> ErrorT [Doc] IO (Env.TypeDict, T.TypeConstraint)
genConstraints interfaces modul =
  do  env <- liftIO $ Env.initialEnvironment (canonicalizeAdts interfaces modul)

      ctors <- forM (Map.keys (Env.constructor env)) $ \name -> do
                 (_, vars, args, result) <- liftIO $ Env.freshDataScheme env name
                 return (name, (vars, foldr (T.==>) result args))

      importedVars <- mapM (canonicalizeValues env) (Map.toList interfaces)

      let allTypes = concat (ctors : importedVars)
          vars = concatMap (fst . snd) allTypes
          header = Map.map snd (Map.fromList allTypes)
          environ = A.noneNoDocs . T.CLet [ T.Scheme vars [] (A.noneNoDocs T.CTrue) header ]

      fvar <- liftIO $ T.variable T.Flexible
      c <- TcExpr.constrain env (program (body modul)) (T.varN fvar)
      return (header, environ c)


canonicalizeValues
    :: Env.Environment
    -> (Module.Name, Interface)
    -> ErrorT [Doc] IO [(String, ([T.Variable], T.Type))]
canonicalizeValues env (moduleName, iface) =
    forM (Map.toList (iTypes iface)) $ \(name,tipe) ->
        do  tipe' <- Env.instantiateType env tipe Map.empty
            return (Module.nameToString moduleName ++ "." ++ name, tipe')


canonicalizeAdts :: Interfaces -> CanonicalModule -> [CanonicalAdt]
canonicalizeAdts interfaces modul =
    localAdts ++ importedAdts
  where
    localAdts :: [CanonicalAdt]
    localAdts = format (Module.names modul, datatypes (body modul))

    importedAdts :: [CanonicalAdt]
    importedAdts = concatMap (format . second iAdts) (Map.toList interfaces)

    format :: (Module.Name, Module.ADTs) -> [CanonicalAdt]
    format (home, adts) =
        map canonical (Map.toList adts)
      where
        canonical :: (String, AdtInfo String) -> CanonicalAdt
        canonical (name, (tvars, ctors)) =
            ( toVar name
            , (tvars, map (first toVar) ctors)
            )

        toVar :: String -> Var.Canonical
        toVar = Var.Canonical (Var.Module home)
