module SourceSyntax.Expression where

import Data.List (intercalate)
import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P
import qualified SourceSyntax.Helpers as Help
import qualified SourceSyntax.Location as Location
import qualified SourceSyntax.Pattern as Pattern
import qualified SourceSyntax.Type as Type
import qualified SourceSyntax.Literal as Literal

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
    | Markdown String String [LExpr t v]
      deriving (Eq)

data Def tipe var
    = Def Pattern.Pattern (LExpr tipe var)
    | TypeAnnotation String Type.Type
      deriving (Eq, Show)

tuple es = Data ("_Tuple" ++ show (length es)) es

delist (Location.L _ (Data "::" [h,t])) = h : delist t
delist _ = []

saveEnvName = "_save_the_environment!!!"

dummyLet defs = 
     Location.none $ Let defs (Location.none $ Var saveEnvName)

instance Show (Expr t v) where
  show = render . pretty

instance Pretty (Expr t v) where
  pretty expr =
   case expr of
     Literal lit -> pretty lit
     Var x -> variable x
     Range e1 e2 -> P.brackets (pretty e1 <> P.text ".." <> pretty e2)
     ExplicitList es -> P.brackets (commaCat (map pretty es))
     Binop "-" (Location.L _ (Literal (Literal.IntNum 0))) e ->
         P.text "-" <> prettyParens e
     Binop op e1 e2 -> P.sep [ prettyParens e1 <+> P.text op', prettyParens e2 ]
         where op' = if Help.isOp op then op else "`" ++ op ++ "`"
     Lambda p e -> let (ps,body) = collectLambdas (Location.none $ Lambda p e)
                   in  P.text "\\" <> P.sep ps <+> P.text "->" <+> pretty body
     App _ _ -> P.hang func 2 (P.sep args)
         where func:args = map prettyParens (collectApps (Location.none expr))
     MultiIf branches ->  P.text "if" $$ nest 3 (vcat $ map iff branches)
         where
           iff (b,e) = P.text "|" <+> P.hang (pretty b <+> P.text "->") 2 (pretty e)
     Let defs e ->
         P.sep [ P.hang (P.text "let") 4 (P.vcat (map pretty defs))
               , P.text "in" <+> pretty e ]
     Case e pats ->
         P.hang pexpr 2 (P.vcat (map pretty' pats))
         where
           pexpr = P.sep [ P.text "case" <+> pretty e, P.text "of" ]
           pretty' (p,e) = pretty p <+> P.text "->" <+> pretty e
     Data "::" [hd,tl] -> pretty hd <+> P.text "::" <+> pretty tl
     Data "[]" [] -> P.text "[]"
     Data name es -> P.hang (P.text name) 2 (P.sep (map prettyParens es))
     Access e x -> prettyParens e <> P.text "." <> variable x
     Remove e x -> P.braces (pretty e <+> P.text "-" <+> variable x)
     Insert (Location.L _ (Remove e y)) x v ->
         P.braces (pretty e <+> P.text "-" <+> variable y <+> P.text "|" <+> variable x <+> P.text "=" <+> pretty v)
     Insert e x v ->
         P.braces (pretty e <+> P.text "|" <+> variable x <+> P.text "=" <+> pretty v)

     Modify e fs ->
         P.braces $ P.hang (pretty e <+> P.text "|")
                           4
                           (commaSep $ map field fs)
       where
         field (x,e) = variable x <+> P.text "<-" <+> pretty e

     Record fs ->
         P.braces $ P.nest 2 (commaSep $ map field fs)
       where
         field (x,e) = variable x <+> P.text "=" <+> pretty e

     Markdown _ _ _ -> P.text "[markdown| ... |]"

instance Pretty (Def t v) where
  pretty def =
   case def of
     TypeAnnotation name tipe ->
         variable name <+> P.text ":" <+> pretty tipe
     Def pattern expr ->
         pretty pattern <+> P.text "=" <+> pretty expr

collectApps lexpr@(Location.L _ expr) =
  case expr of
    App a b -> collectApps a ++ [b]
    _ -> [lexpr]

collectLambdas lexpr@(Location.L _ expr) =
  case expr of
    Lambda pattern body ->
        let (ps, body') = collectLambdas body
        in  (pretty pattern : ps, body')
    _ -> ([], lexpr)

prettyParens (Location.L _ expr) = parensIf needed (pretty expr)
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
