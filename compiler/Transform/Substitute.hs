{-# OPTIONS_GHC -Wall #-}
module Transform.Substitute (subst) where

import Control.Arrow (second, (***))
import SourceSyntax.Expression
import SourceSyntax.Location
import qualified SourceSyntax.Pattern as Pattern
import qualified Data.Set as Set

subst :: String -> Expr -> Expr -> Expr
subst old new expr =
    let f (L s e) = L s (subst old new e) in
    case expr of
      Range e1 e2 -> Range (f e1) (f e2)
      ExplicitList es -> ExplicitList (map f es)
      Binop op e1 e2 -> Binop op (f e1) (f e2)
      Lambda p e
          | Set.member old (Pattern.boundVars p) -> expr
          | otherwise -> Lambda p (f e)
      App e1 e2 -> App (f e1) (f e2)
      MultiIf ps -> MultiIf (map (f *** f) ps)

      Let defs body
          | anyShadow -> expr
          | otherwise -> Let (map substDef defs) (f body)
        where
          substDef (Definition p e t) = Definition p (f e) t
          anyShadow =
              any (Set.member old . Pattern.boundVars) [ p | Definition p _ _ <- defs ]

      Var x -> if x == old then new else expr
      Case e cases -> Case (f e) $ map (second f) cases
      Data name es -> Data name (map f es)
      Access e x -> Access (f e) x
      Remove e x -> Remove (f e) x
      Insert e x v -> Insert (f e) x (f v)
      Modify r fs -> Modify (f r) (map (second f) fs)
      Record fs -> Record (map (second f) fs)
      Literal _ -> expr
      Markdown uid md es -> Markdown uid md (map f es)
      PortIn name st -> PortIn name st
      PortOut name st signal -> PortOut name st (f signal)