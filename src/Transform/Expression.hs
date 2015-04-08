{-# OPTIONS_GHC -Wall #-}
module Transform.Expression (crawlLet) where

import Control.Applicative ((<$>),(<*>))
import AST.Annotation ( Annotated(A) )
import AST.Expression.General


crawlLet
    :: ([def] -> Either a [def'])
    -> Expr ann def var
    -> Either a (Expr ann def' var)
crawlLet defsTransform annotatedExpression =
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

          Port impl ->
              Port <$>
                  case impl of
                    In name tipe ->
                        return (In name tipe)

                    Out name expr tipe ->
                        do  expr' <- go expr
                            return (Out name expr' tipe)

                    Task name expr tipe ->
                        do  expr' <- go expr
                            return (Task name expr' tipe)
