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
crawl inputCheck outputCheck loopbackCheck defsTransform expression =
    go expression
  where
    go (A srcSpan expr) =
        A srcSpan <$>
        case expr of
          Var x ->
              return (Var x)

          Lambda p e ->
              Lambda p <$> go e

          Binop op e1 e2 ->
              Binop op <$> go e1 <*> go e2

          Case e cases ->
              Case <$> go e <*> mapM (\(p,b) -> (,) p <$> go b) cases

          Data name es ->
              Data name <$> mapM go es

          Literal lit ->
              return (Literal lit)

          Range e1 e2 ->
              Range <$> go e1 <*> go e2

          ExplicitList es ->
              ExplicitList <$> mapM go es

          App e1 e2 ->
              App <$> go e1 <*> go e2

          MultiIf branches ->
              MultiIf <$> mapM (\(b,e) -> (,) <$> go b <*> go e) branches

          Access e lbl ->
              Access <$> go e <*> return lbl

          Remove e lbl ->
              Remove <$> go e <*> return lbl

          Insert e lbl v ->
              Insert <$> go e <*> return lbl <*> go v

          Modify e fields ->
              Modify <$> go e <*> mapM (\(k,v) -> (,) k <$> go v) fields

          Record fields ->
              Record <$> mapM (\(k,v) -> (,) k <$> go v) fields

          Let defs body ->
              Let <$> defsTransform defs <*> go body

          GLShader uid src gltipe ->
              return $ GLShader uid src gltipe

          Input name st ->
              do  inputCheck name st
                  return $ Input name st

          Output name st signal ->
              do  outputCheck name st
                  Output name st <$> go signal

          Loopback name st maybeExpr ->
              do  let hasExpr = maybe False (const True) maybeExpr
                  loopbackCheck name st hasExpr
                  case maybeExpr of
                    Nothing ->
                        return (Loopback name st Nothing)
                    Just expr ->
                        (Loopback name st . Just) <$> go expr
