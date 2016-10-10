module Type.Inference where

import Control.Arrow (first, second)
import Control.Monad.Except (Except, forM, liftIO, runExceptT, throwError)
import qualified Data.Map as Map
import qualified Data.Traversable as Traverse

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Type.Constrain.Expression as TcExpr
import qualified Type.Environment as Env
import qualified Type.Solve as Solve
import qualified Type.State as TS
import qualified Type.Type as T

import System.IO.Unsafe
    -- Maybe possible to switch over to ST instead of IO.
    -- I don't think that'd be worthwhile though.


infer
    :: Module.Interfaces
    -> Module.Canonical
    -> Except [A.Located Error.Error] (Map.Map String Type.Canonical)
infer interfaces modul =
  either throwError return $ unsafePerformIO $ runExceptT $
    do  (header, constraint) <-
            liftIO (genConstraints interfaces modul)

        state <- Solve.solve constraint

        let header' = Map.delete "::" header
        let types = Map.map A.drop (Map.difference (TS.sSavedEnv state) header')

        liftIO (Traverse.traverse T.toSrcType types)


genConstraints
    :: Module.Interfaces
    -> Module.Canonical
    -> IO (Map.Map String T.Type, T.TypeConstraint)
genConstraints interfaces modul =
  do  env <- Env.initialize (canonicalizeUnions interfaces modul)

      ctors <-
          forM (Env.ctorNames env) $ \name ->
            do  (_, vars, args, result) <- Env.freshDataScheme env name
                return (name, (vars, foldr (T.==>) result args))

      importedVars <-
          mapM (canonicalizeValues env) (Map.toList interfaces)

      let allTypes = concat (ctors : importedVars)
      let vars = concatMap (fst . snd) allTypes
      let header = Map.map snd (Map.fromList allTypes)
      let environ = T.CLet [ T.Scheme vars [] T.CTrue (Map.map (A.A undefined) header) ]

      fvar <- T.mkVar Nothing

      constraint <-
          TcExpr.constrain env (Module.program (Module.info modul)) (T.VarN fvar)

      return (header, environ constraint)


canonicalizeValues
    :: Env.Env
    -> (ModuleName.Canonical, Module.Interface)
    -> IO [(String, ([T.Variable], T.Type))]
canonicalizeValues env (moduleName, iface) =
    forM (Map.toList (Module.iTypes iface)) $ \(name, tipe) ->
        do  (flexType, flexVars) <- Env.instantiateType T.Flex env tipe
            return
              ( ModuleName.canonicalToString moduleName ++ "." ++ name
              , ( Map.elems flexVars
                , flexType
                )
              )


canonicalizeUnions :: Module.Interfaces -> Module.Canonical -> [Module.CanonicalUnion]
canonicalizeUnions interfaces (Module.Module name _ info) =
    localUnions ++ importedUnions
  where
    localUnions :: [Module.CanonicalUnion]
    localUnions =
      format (name, Module.unions info)

    importedUnions :: [Module.CanonicalUnion]
    importedUnions =
      concatMap (format . second Module.iUnions) (Map.toList interfaces)


format :: (ModuleName.Canonical, Module.Unions) -> [Module.CanonicalUnion]
format (home, adts) =
    map canonical (Map.toList adts)
  where
    canonical :: (String, Module.UnionInfo String) -> Module.CanonicalUnion
    canonical (name, (tvars, ctors)) =
        ( toVar name
        , (tvars, map (first toVar) ctors)
        )

    toVar :: String -> Var.Canonical
    toVar name =
        Var.fromModule home name
