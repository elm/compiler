{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Body (flatten) where

import qualified AST.Declaration as D
import qualified AST.Effects as Fx
import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Canonicalize.Sort as Sort
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



flatten :: ModuleName.Canonical -> [D.Canonical] -> Fx.Effects -> Canonical.Expr
flatten moduleName decls effects =
  let
    declDefs =
      concatMap (declToDefs moduleName) decls

    effectDefs =
      effectsToDefs moduleName effects

    loc =
      A.A (error "this annotation should never be needed")
  in
    Sort.definitions $ loc $
      E.Let
        (declDefs ++ effectDefs)
        (loc $ E.SaveEnv moduleName effects)



-- DECLARATIONS


declToDefs :: ModuleName.Canonical -> D.Canonical -> [Canonical.Def]
declToDefs moduleName (A.A (region,_) decl) =
  let
    typeVar =
      Var.fromModule moduleName

    loc expr =
      A.A region expr
  in
  case decl of
    D.Def def ->
      [def]

    D.Union name tvars constructors ->
      let
        tagToDef (ctor, tipes) =
          let
            vars =
              take (length tipes) infiniteArgs

            tbody =
              T.App (T.Type (typeVar name)) (map T.Var tvars)

            body =
              loc . E.Data ctor $ map (loc . E.localVar) vars
          in
            definition ctor (buildFunction body vars) region (foldr T.Lambda tbody tipes)
      in
        map tagToDef constructors

    D.Alias name tvars tipe@(T.Record fields Nothing) ->
        [ definition name (buildFunction record vars) region (foldr T.Lambda result args) ]
      where
        result =
          T.Aliased (typeVar name) (zip tvars (map T.Var tvars)) (T.Holey tipe)

        args =
          map snd fields

        vars =
          take (length args) infiniteArgs

        record =
          loc (E.Record (zip (map fst fields) (map (loc . E.localVar) vars)))

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    D.Alias _ _ _ ->
      []

    -- no constraints are needed for fixity declarations
    D.Fixity _ _ _ ->
      []


infiniteArgs :: [String]
infiniteArgs =
  map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show (n :: Int)) [1..]


buildFunction :: Canonical.Expr -> [String] -> Canonical.Expr
buildFunction body@(A.A ann _) vars =
  foldr
      (\pattern expr -> A.A ann (E.Lambda pattern expr))
      body
      (map (A.A ann . P.Var) vars)


definition :: String -> Canonical.Expr -> R.Region -> T.Canonical -> Canonical.Def
definition name expr@(A.A ann _) region tipe =
  Canonical.Definition
    Canonical.dummyFacts
    (A.A ann (P.Var name))
    expr
    (Just (A.A region tipe))



-- EFFECTS


effectsToDefs :: ModuleName.Canonical -> Fx.Effects -> [Canonical.Def]
effectsToDefs moduleName effects =
  case effects of
    Fx.None ->
      []

    Fx.Foreign ->
      []

    Fx.Effect info ->
      case Fx._type info of
        Fx.CmdManager cmd ->
          [ effectsToDefsHelp "command" (E.Cmd moduleName) cmd ]

        Fx.SubManager sub ->
          [ effectsToDefsHelp "subscription" (E.Sub moduleName) sub ]

        Fx.FxManager cmd sub ->
          [ effectsToDefsHelp "command" (E.Cmd moduleName) cmd
          , effectsToDefsHelp "subscription" (E.Sub moduleName) sub
          ]


effectsToDefsHelp
  :: String
  -> (String -> Canonical.Expr')
  -> A.Located String
  -> Canonical.Def
effectsToDefsHelp name toExpr' (A.A region typeName) =
  Canonical.Definition
    Canonical.dummyFacts
    (A.A region (P.Var name))
    (A.A region (toExpr' typeName))
    Nothing
