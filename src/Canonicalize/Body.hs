{-# OPTIONS_GHC -Wall #-}
module Canonicalize.Body (flatten) where

import qualified Data.Maybe as Maybe

import qualified AST.Declaration as Decl
import qualified AST.Effects as Effects
import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Canonicalize.Sort as Sort
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



flatten :: ModuleName.Canonical -> Decl.Canonical -> Effects.Canonical -> Canonical.Expr
flatten moduleName (Decl.Decls defs unions aliases _) effects =
  let
    allDefs =
      concat
        [ concatMap (unionToDefs moduleName) unions
        , Maybe.mapMaybe (aliasToDefs moduleName) aliases
        , effectsToDefs moduleName effects
        , map A.drop defs
        ]

    loc =
      A.A (error "this annotation should never be needed")
  in
    Sort.definitions $ loc $
      E.Let allDefs (loc $ E.SaveEnv moduleName effects)



-- TYPES TO DEFS


unionToDefs :: ModuleName.Canonical -> A.Commented (Decl.Union Type.Canonical) -> [Canonical.Def]
unionToDefs moduleName (A.A (region,_) (Decl.Type name tvars constructors)) =
  let
    tagToDef (tag, tipes) =
      let
        vars =
          take (length tipes) infiniteArgs

        tbody =
          Type.App
            (Type.Type (Var.fromModule moduleName name))
            (map Type.Var tvars)

        body =
          A.A region $
            E.Data tag (map (A.A region . E.localVar) vars)
      in
        definition tag (buildFunction vars body) region (foldr Type.Lambda tbody tipes)
  in
    map tagToDef constructors


aliasToDefs :: ModuleName.Canonical -> A.Commented (Decl.Alias Type.Canonical) -> Maybe Canonical.Def
aliasToDefs moduleName (A.A (region,_) (Decl.Type name tvars tipe)) =
  case tipe of
    Type.Record fields Nothing ->
      let
        resultType =
          Type.Aliased
            (Var.fromModule moduleName name)
            (zip tvars (map Type.Var tvars))
            (Type.Holey tipe)

        args =
          map snd fields

        vars =
          take (length args) infiniteArgs

        record =
          A.A region $
            E.Record (zip (map fst fields) (map (A.A region . E.localVar) vars))
      in
        Just $ definition name (buildFunction vars record) region (foldr Type.Lambda resultType args)

    _ ->
      Nothing


infiniteArgs :: [String]
infiniteArgs =
  map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show (n :: Int)) [1..]


buildFunction :: [String] -> Canonical.Expr -> Canonical.Expr
buildFunction vars body@(A.A ann _) =
  foldr
    (\pattern expr -> A.A ann (E.Lambda pattern expr))
    body
    (map (A.A ann . P.Var) vars)


definition :: String -> Canonical.Expr -> R.Region -> Type.Canonical -> Canonical.Def
definition name expr@(A.A ann _) region tipe =
  Canonical.Def
    Canonical.dummyFacts
    (A.A ann (P.Var name))
    expr
    (Just (A.A region tipe))



-- EFFECTS


effectsToDefs :: ModuleName.Canonical -> Effects.Canonical -> [Canonical.Def]
effectsToDefs moduleName effects =
  case effects of
    Effects.None ->
      []

    Effects.Foreign foreigns ->
      map foreignToDef foreigns

    Effects.Manager info ->
      let
        cmdToDef (A.A region name) =
          definition
            "command"
            (A.A region (E.Cmd moduleName))
            region
            (Type.cmd moduleName name)

        subToDef (A.A region name) =
          definition
            "subscription"
            (A.A region (E.Sub moduleName))
            region
            (Type.sub moduleName name)
      in
        case Effects._managerType info of
          Effects.CmdManager cmd ->
            [ cmdToDef cmd
            ]

          Effects.SubManager sub ->
            [ subToDef sub
            ]

          Effects.FxManager cmd sub ->
            [ cmdToDef cmd
            , subToDef sub
            ]


foreignToDef :: A.Commented Effects.ForeignCanonical -> Canonical.Def
foreignToDef (A.A (region, _) (Effects.ForeignCanonical name kind tipe)) =
  definition name (A.A region (toForeignExpr name kind)) region tipe


toForeignExpr :: String -> Effects.Kind -> Canonical.Expr'
toForeignExpr name kind =
  case kind of
    Effects.Cmd tipe ->
      E.ForeignCmd name tipe

    Effects.Sub tipe ->
      E.ForeignSub name tipe

