{-# OPTIONS_GHC -Wall #-}
module Transform.Expression (crawlLet, checkWires) where

import Control.Applicative ((<$>),(<*>))
import AST.Annotation ( Annotated(A) )
import AST.Expression.General
import qualified AST.Expression.Canonical as Canonical
import AST.Type (Type, CanonicalType)


crawlLet
    :: ([def] -> Either a [def'])
    -> Expr ann def var
    -> Either a (Expr ann def' var)
crawlLet =
    crawl
      (\_ _ -> return ())
      (\_ _ -> return ())
      (\_ _ _ -> return ())


checkWires
    :: (String -> CanonicalType -> Either a ())
    -> (String -> CanonicalType -> Either a ())
    -> (String -> CanonicalType -> Bool -> Either a ())
    -> Canonical.Expr
    -> Either a Canonical.Expr
checkWires inputCheck outputCheck loopbackCheck expr =
    crawl inputCheck outputCheck loopbackCheck (mapM checkDef) expr
  where
    checkDef def@(Canonical.Definition _ body _) =
        do  _ <- checkWires inputCheck outputCheck loopbackCheck body
            return def


crawl
    :: (String -> Type var -> Either a ())
    -> (String -> Type var -> Either a ())
    -> (String -> Type var -> Bool -> Either a ())
    -> ([def] -> Either a [def'])
    -> Expr ann def var
    -> Either a (Expr ann def' var)
crawl inputCheck outputCheck loopbackCheck defsTransform annotatedExpression =
    go annotatedExpression
  where
    go (A srcSpan expression) =
        A srcSpan <$>
        case expression of
          Var x ->
              return (Var x)

          Lambda pattern body ->
              Lambda pattern <$> go body

          Binop op leftExpr rightExpr ->
              Binop op <$> go leftExpr <*> go rightExpr

          Case e cases ->
              Case <$> go e <*> mapM (\(p,b) -> (,) p <$> go b) cases

          Data name es ->
              Data name <$> mapM go es

          Literal lit ->
              return (Literal lit)

          Range lowExpr highExpr ->
              Range <$> go lowExpr <*> go highExpr

          ExplicitList expressions ->
              ExplicitList <$> mapM go expressions

          App funcExpr argExpr ->
              App <$> go funcExpr <*> go argExpr

          MultiIf branches ->
              MultiIf <$> mapM (\(b,e) -> (,) <$> go b <*> go e) branches

          Access record field ->
              Access <$> go record <*> return field

          Remove record field ->
              Remove <$> go record <*> return field

          Insert record field expr ->
              Insert <$> go record <*> return field <*> go expr

          Modify record fields ->
              Modify
                <$> go record
                <*> mapM (\(field,expr) -> (,) field <$> go expr) fields

          Record fields ->
              Record <$> mapM (\(field,expr) -> (,) field <$> go expr) fields

          Let defs body ->
              Let <$> defsTransform defs <*> go body

          GLShader uid src gltipe ->
              return $ GLShader uid src gltipe

          Input name st ->
              do  inputCheck name st
                  return $ Input name st

          Output name st expr ->
              do  outputCheck name st
                  Output name st <$> go expr

          Loopback name st maybeExpr ->
              do  let hasExpr = maybe False (const True) maybeExpr
                  loopbackCheck name st hasExpr
                  case maybeExpr of
                    Nothing ->
                        return (Loopback name st Nothing)
                    Just expr ->
                        (Loopback name st . Just) <$> go expr
