{-# OPTIONS_GHC -Wall #-}
module Transform.Declaration where

import Control.Applicative ((<$>))
import qualified SourceSyntax.Pattern as P
import SourceSyntax.Expression as E
import SourceSyntax.Declaration as D

import qualified Transform.Expression as Expr
import qualified Transform.Definition as Def


combineAnnotations :: [ParseDeclaration] -> Either String [Declaration]
combineAnnotations = go
    where
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."

      exprCombineAnnotations = Expr.crawlLet Def.combineAnnotations

      go decls =
          case decls of
            -- simple cases, pass them through with no changes
            [] -> return []

            Datatype name tvars ctors ds : rest ->
                (:) (Datatype name tvars ctors ds) <$> go rest

            TypeAlias name tvars alias ds : rest ->
                (:) (TypeAlias name tvars alias ds) <$> go rest

            Fixity assoc prec op : rest ->
                (:) (Fixity assoc prec op) <$> go rest

            -- combine definitions
            D.Definition def : defRest ->
                case def of
                  Def pat expr ->
                      do expr' <- exprCombineAnnotations expr
                         let def' = E.Definition pat expr' Nothing
                         (:) (D.Definition def') <$> go defRest

                  TypeAnnotation name tipe ->
                      case defRest of
                        D.Definition (Def pat@(P.PVar name') expr) : rest | name == name' ->
                            do expr' <- exprCombineAnnotations expr
                               let def' = E.Definition pat expr' (Just tipe)
                               (:) (D.Definition def') <$> go rest

                        _ -> Left (msg name)

            -- combine ports
            Port port : portRest ->
                case port of
                  PPDef name _ -> Left (msg name)
                  PPAnnotation name tipe ->
                      case portRest of
                        Port (PPDef name' expr) : rest | name == name' ->
                            do expr' <- exprCombineAnnotations expr
                               (:) (Port (Out name expr' tipe)) <$> go rest

                        _ -> (:) (Port (In name tipe)) <$> go portRest

