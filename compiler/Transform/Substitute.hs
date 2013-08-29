{-# OPTIONS_GHC -Wall #-}
module Transform.Substitute (subst) where

import SourceSyntax.Expression
import SourceSyntax.Location
import Control.Arrow (second, (***))

subst :: String -> Expr t v -> Expr t v -> Expr t v
subst old new expr =
    let f (L s e) = L s (subst old new e) in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      ExplicitList es -> ExplicitList (map f es)
      Binop op e1 e2 -> Binop op (f e1) (f e2)
      Lambda p e -> Lambda p (f e)
      App e1 e2 -> App (f e1) (f e2)
      MultiIf ps -> MultiIf (map (f *** f) ps)
      Let defs body -> Let (map substDef defs) (f body)
              where substDef (Def name e)  = Def name (f e)
                    substDef anno@(TypeAnnotation _ _) = anno
      Var x -> if x == old then new else expr
      Case e cases -> Case (f e) $ map (second f) cases
      Data name es -> Data name (map f es)
      Access e x -> Access (f e) x
      Remove e x -> Remove (f e) x
      Insert e x v -> Insert (f e) x (f v)
      Modify r fs -> Modify (f r) (map (second f) fs)
      Record fs -> Record (map (second f) fs)
      Literal _ -> expr
      Markdown md es -> Markdown md (map f es)
