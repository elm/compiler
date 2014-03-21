{-# OPTIONS_GHC -Wall #-}
module Transform.Expression (crawlLet, checkPorts) where

import Control.Applicative ((<$>),(<*>))
import SourceSyntax.Annotation ( Annotated(A) )
import SourceSyntax.Expression
import SourceSyntax.Type (Type)

crawlLet :: ([def] -> Either a [def'])
         -> GeneralExpr ann def var
         -> Either a (GeneralExpr ann def' var)
crawlLet = crawl (\_ _ -> return ()) (\_ _ -> return ())

checkPorts :: (String -> Type -> Either a ())
           -> (String -> Type -> Either a ())
           -> Expr
           -> Either a Expr
checkPorts inCheck outCheck expr =
    crawl inCheck outCheck (mapM checkDef) expr
    where
      checkDef def@(Definition _ body _) =
          do _ <- checkPorts inCheck outCheck body
             return def

crawl :: (String -> Type -> Either a ())
      -> (String -> Type -> Either a ())
      -> ([def] -> Either a [def'])
      -> GeneralExpr ann def var
      -> Either a (GeneralExpr ann def' var)
crawl portInCheck portOutCheck defsTransform = go
    where
      go (A srcSpan expr) =
          A srcSpan <$>
          case expr of
            Var x -> return (Var x)
            Lambda p e -> Lambda p <$> go e
            Binop op e1 e2 -> Binop op <$> go e1 <*> go e2
            Case e cases -> Case <$> go e <*> mapM (\(p,b) -> (,) p <$> go b) cases
            Data name es -> Data name <$> mapM go es
            Literal lit -> return (Literal lit)
            Range e1 e2 -> Range <$> go e1 <*> go e2
            ExplicitList es -> ExplicitList <$> mapM go es
            App e1 e2 -> App <$> go e1 <*> go e2
            MultiIf branches -> MultiIf <$> mapM (\(b,e) -> (,) <$> go b <*> go e) branches
            Access e lbl -> Access <$> go e <*> return lbl
            Remove e lbl -> Remove <$> go e <*> return lbl
            Insert e lbl v -> Insert <$> go e <*> return lbl <*> go v
            Modify e fields -> Modify <$> go e <*> mapM (\(k,v) -> (,) k <$> go v) fields
            Record fields -> Record <$> mapM (\(k,v) -> (,) k <$> go v) fields
            Markdown uid md es -> Markdown uid md <$> mapM go es
            Let defs body -> Let <$> defsTransform defs <*> go body
            PortIn name st ->
                do portInCheck name st
                   return $ PortIn name st
            PortOut name st signal ->
                do portOutCheck name st
                   PortOut name st <$> go signal
