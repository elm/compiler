{-# LANGUAGE DeriveDataTypeable #-}
module SourceSyntax.Expression where

import Data.Data
import Data.List (intercalate)
import qualified Text.Pandoc as Pandoc
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as PP
import qualified SourceSyntax.Helpers as Help
import qualified SourceSyntax.Location as Location
import qualified SourceSyntax.Pattern as Pattern
import qualified SourceSyntax.Literal as Literal
import Types.Types

type LExpr tipe var = Location.Located (Expr tipe var)
data Expr t v
    = Literal Literal.Literal
    | Var String
    | Range (LExpr t v) (LExpr t v)
    | ExplicitList [LExpr t v]
    | Binop String (LExpr t v) (LExpr t v)
    | Lambda Pattern.Pattern (LExpr t v)
    | App (LExpr t v) (LExpr t v)
    | MultiIf [(LExpr t v,LExpr t v)]
    | Let [Def t v] (LExpr t v)
    | Case (LExpr t v) [(Pattern.Pattern, LExpr t v)]
    | Data String [LExpr t v]
    | Access (LExpr t v) String
    | Remove (LExpr t v) String
    | Insert (LExpr t v) String (LExpr t v)
    | Modify (LExpr t v) [(String, LExpr t v)]
    | Record [(String, LExpr t v)]
    | Markdown Pandoc.Pandoc
      deriving (Eq, Data, Typeable, Show)

data Def tipe var
    = Def Pattern.Pattern (LExpr tipe var)
    | TypeAnnotation String Type
      deriving (Eq, Data, Typeable, Show)

tuple es = Data ("Tuple" ++ show (length es)) es

delist (Location.L _ _ (Data "::" [h,t])) = h : delist t
delist _ = []

instance Pretty (Expr t v) where
  pretty expr =
   case expr of
     Literal lit -> pretty lit
     Var x@(c:_) -> parensIf (Help.isOp c) (PP.text x)
     Range e1 e2 -> PP.brackets (pretty e1 <> PP.text ".." <> pretty e2)
     ExplicitList es -> PP.brackets (commaCat (map pretty es))
     Binop op e1 e2 -> PP.sep [ prettyParens e1 <+> PP.text op, prettyParens e2 ]
     Lambda p e -> let (ps,body) = collectLambdas (Location.none $ Lambda p e)
                   in  PP.text "\\" <> PP.sep ps <+> PP.text "->" <+> pretty body
     App _ _ -> PP.hang func 2 (PP.sep args)
       where func:args = map prettyParens (collectApps (Location.none expr))
     MultiIf branches ->  PP.text "if" $$ nest 3 (vcat $ map iff branches)
         where
           iff (b,e) = PP.text "|" <+> PP.hang (pretty b <+> PP.text "->") 2 (pretty e)
     Let defs e ->
         PP.sep [ PP.hang (PP.text "let") 4 (PP.vcat (map pretty defs))
                , PP.text "in" <+> pretty e ]
     Case e pats ->
         PP.hang pexpr 2 (PP.vcat (map pretty' pats))
         where
           pexpr = PP.sep [ PP.text "case" <+> pretty e, PP.text "of" ]
           pretty' (p,e) = pretty p <+> PP.text "->" <+> pretty e
     Data "::" [hd,tl] -> pretty hd <+> PP.text "::" <+> pretty tl
     Data "[]" [] -> PP.text "[]"
     Data name es -> PP.hang (PP.text name) 2 (PP.sep (map prettyParens es))
     Access e x -> prettyParens e <> PP.text "." <> PP.text x
     Remove e x -> PP.braces (pretty e <+> PP.text "-" <+> PP.text x)
     Insert (Location.L _ _ (Remove e y)) x v ->
         PP.braces (pretty e <+> PP.text "-" <+> PP.text y <+> PP.text "|" <+> PP.text x <+> PP.text "=" <+> pretty v)
     Insert e x v ->
         PP.braces (pretty e <+> PP.text "|" <+> PP.text x <+> PP.text "=" <+> pretty v)

     Modify e fs ->
         PP.braces $ PP.hang (pretty e <+> PP.text "|")
                             4
                             (PP.sep . PP.punctuate PP.comma $ map field fs)
       where
         field (x,e) = PP.text x <+> PP.text "<-" <+> pretty e

     Record fs ->
         PP.braces $ PP.nest 2 (PP.sep . PP.punctuate PP.comma $ map field fs)
       where
         field (x,e) = PP.text x <+> PP.text "=" <+> pretty e

     Markdown _ -> PP.text "[markdown| ... |]"

instance Pretty (Def t v) where
  pretty def =
   case def of
     TypeAnnotation name tipe ->
         PP.text name <+> PP.text ":" <+> PP.text (show tipe)
     Def pattern expr ->
         pretty pattern <+> PP.text "=" <+> pretty expr

collectApps lexpr@(Location.L _ _ expr) =
  case expr of
    App a b -> collectApps a ++ [b]
    _ -> [lexpr]

collectLambdas lexpr@(Location.L _ _ expr) =
  case expr of
    Lambda pattern body ->
        let (ps, body') = collectLambdas body
        in  (pretty pattern : ps, body')
    _ -> ([], lexpr)

prettyParens (Location.L _ _ expr) = parensIf needed (pretty expr)
  where
    needed =
      case expr of
        Binop _ _ _ -> True
        Lambda _ _  -> True
        App _ _     -> True
        MultiIf _   -> True
        Let _ _     -> True
        Case _ _    -> True
        Data name (x:xs) -> name /= "::"
        _ -> False
