{-# OPTIONS_GHC -Wall #-}
module Transform.Definition where

import Control.Applicative ((<$>))
import qualified AST.Pattern as P
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified Transform.Expression as Expr

combineAnnotations :: [Source.Def] -> Either String [Valid.Def]
combineAnnotations = go
    where
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."

      exprCombineAnnotations = Expr.crawlLet combineAnnotations

      go defs =
          case defs of
            Source.TypeAnnotation name tipe : rest ->
                case rest of
                  Source.Definition pat@(P.Var name') expr : rest'
                      | name == name' ->
                          do expr' <- exprCombineAnnotations expr
                             let def = Valid.Definition pat expr' (Just tipe)
                             (:) def <$> go rest'

                  _ -> Left (msg name)

            Source.Definition pat expr : rest ->
                do expr' <- exprCombineAnnotations expr
                   let def = Valid.Definition pat expr' Nothing
                   (:) def <$> go rest

            [] -> return []
