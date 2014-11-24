module Transform.Interface (filterExports) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified AST.Module as Module
import qualified AST.Variable as Var


filterExports :: Module.Interface -> Module.Interface
filterExports interface =
    interface
    { Module.iTypes =
        Map.fromList $
          Maybe.mapMaybe
            (get (Module.iTypes interface))
            (Var.getValues exports ++ ctors)

    , Module.iAliases =
        Map.fromList $
          Maybe.mapMaybe
            (get (Module.iAliases interface))
            (Var.getAliases exports)

    , Module.iAdts =
        Map.fromList
          (Maybe.mapMaybe (trimUnions interface) unions)
    }
  where
    exports :: [Var.Value]
    exports =
        Module.iExports interface

    unions :: [(String, Var.Listing String)]
    unions =
        Var.getUnions exports

    ctors :: [String]
    ctors =
        concatMap (\(_, Var.Listing ctorList _) -> ctorList) unions


get :: Map.Map String a -> String -> Maybe (String, a)
get dict key =
  case Map.lookup key dict of
    Nothing -> Nothing
    Just value -> Just (key, value)


trimUnions
    :: Module.Interface
    -> (String, Var.Listing String)
    -> Maybe (String, Module.AdtInfo String)
trimUnions interface (name, Var.Listing exportedCtors _) =
  case Map.lookup name (Module.iAdts interface) of
    Nothing -> Nothing
    Just (tvars, ctors) ->
      let isExported (ctor, _) =
            ctor `elem` exportedCtors
      in
          Just (name, (tvars, filter isExported ctors))
