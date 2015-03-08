{-# OPTIONS_GHC -Wall #-}
module Transform.Declaration (combineAnnotations, toExpr) where

import Control.Applicative ((<$>))

import qualified AST.Annotation as A
import qualified AST.Declaration as D
import qualified AST.Expression.General as E
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Variable as Var

import qualified Transform.Expression as Expr
import qualified Transform.Definition as Def


combineAnnotations :: [D.SourceDecl] -> Either String [D.ValidDecl]
combineAnnotations =
    go
  where
    errorMessage kind name =
        "Syntax Error: The type annotation for " ++ kind ++ " '" ++
        name ++ "' must be directly above its definition."

    exprCombineAnnotations =
        Expr.crawlLet Def.combineAnnotations

    go decls =
        case decls of
          -- simple cases, pass them through with no changes
          [] ->
              return []

          D.Datatype name tvars ctors : rest ->
              (:) (D.Datatype name tvars ctors) <$> go rest

          D.TypeAlias name tvars alias : rest ->
              (:) (D.TypeAlias name tvars alias) <$> go rest

          D.Fixity assoc prec op : rest ->
              (:) (D.Fixity assoc prec op) <$> go rest

          -- combine definitions
          D.Definition def : defRest ->
              case def of
                Source.Definition pat expr ->
                    do  expr' <- exprCombineAnnotations expr
                        let def' = Valid.Definition pat expr' Nothing
                        (:) (D.Definition def') <$> go defRest

                Source.TypeAnnotation name tipe ->
                    case defRest of
                      D.Definition (Source.Definition pat@(P.Var name') expr) : rest
                          | name == name' ->
                              do  expr' <- exprCombineAnnotations expr
                                  let def' = Valid.Definition pat expr' (Just tipe)
                                  (:) (D.Definition def') <$> go rest

                      _ -> Left (errorMessage "value" name)

          -- combine wires
          D.Wire wire : wireRest ->
              case wire of
                D.InputAnnotation name tipe ->
                    (:) (D.Wire (D.Input name tipe)) <$> go wireRest

                D.OutputDefinition name _ ->
                    Left (errorMessage "output" name)

                D.OutputAnnotation name tipe ->
                    case wireRest of
                      D.Wire (D.OutputDefinition name' expr) : rest | name == name' ->
                          do  expr' <- exprCombineAnnotations expr
                              (:) (D.Wire (D.Output name expr' tipe)) <$> go rest

                      _ ->
                          Left (errorMessage "output" name)

                D.LoopbackDefinition name _tipe ->
                    Left (errorMessage "loopback" name)

                D.LoopbackAnnotation name tipe ->
                    case wireRest of
                      D.Wire (D.LoopbackDefinition name' expr) : rest | name == name' ->
                          do  expr' <- exprCombineAnnotations expr
                              let loop = D.ValidLoopback name (Just expr') tipe
                              (:) (D.Wire (D.Loopback loop)) <$> go rest

                      _ ->
                          do  let loop = D.ValidLoopback name Nothing tipe
                              (:) (D.Wire (D.Loopback loop)) <$> go wireRest


toExpr :: Module.Name -> [D.CanonicalDecl] -> [Canonical.Def]
toExpr moduleName =
  concatMap (toDefs moduleName)


toDefs :: Module.Name -> D.CanonicalDecl -> [Canonical.Def]
toDefs moduleName decl =
  let typeVar = Var.Canonical (Var.Module moduleName) in
  case decl of
    D.Definition def ->
        [def]

    D.Datatype name tvars constructors ->
        concatMap toDefs' constructors
      where
        toDefs' (ctor, tipes) =
            let vars = take (length tipes) arguments
                tbody = T.App (T.Type (typeVar name)) (map T.Var tvars)
                body = A.none . E.Data ctor $ map (A.none . E.localVar) vars
            in
                [ definition ctor (buildFunction body vars) (foldr T.Lambda tbody tipes) ]

    D.TypeAlias name tvars tipe@(T.Record fields ext) ->
        [ definition name (buildFunction record vars) (foldr T.Lambda result args) ]
      where
        result =
          T.Aliased (typeVar name) (zip tvars (map T.Var tvars)) tipe

        args =
          map snd fields ++ maybe [] (:[]) ext

        var = A.none . E.localVar
        vars = take (length args) arguments

        efields = zip (map fst fields) (map var vars)
        record =
          case ext of
            Nothing -> A.none $ E.Record efields
            Just _ ->
                foldl (\r (f,v) -> A.none $ E.Insert r f v) (var $ last vars) efields

    -- Type aliases must be added to an extended equality dictionary,
    -- but they do not require any basic constraints.
    D.TypeAlias _ _ _ ->
        []

    D.Wire wire ->
        case wire of
          D.Output name expr@(A.A s _) tipe ->
              [ definition name (A.A s $ E.Output name tipe expr) tipe ]

          D.Input name tipe ->
              [ definition name (A.none $ E.Input name tipe) tipe ]

          D.Loopback loopback ->
              case loopback of
                D.Mailbox name tipe ->
                    [ definition name (A.none $ E.LoopbackIn name E.Mailbox) tipe ]

                D.Promise name promiseType expr resultType ->
                    let dummyName =
                          "$" ++ name ++ "$effect"
                    in
                        [ definition name (A.none $ E.LoopbackIn name E.PromiseStream) resultType
                        , definition dummyName (A.none $ E.LoopbackOut name expr) promiseType
                        ]

    -- no constraints are needed for fixity declarations
    D.Fixity _ _ _ ->
        []


arguments :: [String]
arguments =
  map (:[]) ['a'..'z'] ++ map (\n -> "_" ++ show (n :: Int)) [1..]


buildFunction :: Canonical.Expr -> [String] -> Canonical.Expr
buildFunction body@(A.A s _) vars =
  foldr (\p e -> A.A s (E.Lambda p e)) body (map P.Var vars)


definition :: String -> Canonical.Expr -> T.CanonicalType -> Canonical.Def
definition name expr tipe =
  Canonical.Definition (P.Var name) expr (Just tipe)
