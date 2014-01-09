{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Expression where

import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P
import qualified SourceSyntax.Helpers as Help
import qualified SourceSyntax.Location as Location
import qualified SourceSyntax.Pattern as Pattern
import qualified SourceSyntax.Type as SrcType
import qualified SourceSyntax.Literal as Literal
import qualified Type.Type as Type

{-| This is a located expression. -}
type LExpr' def = Location.Located (Expr' def)

{-| This is a fully general expression. The compilation process will enrich
this data structure with additional information after the initial parse.
The type holes let us reflect these structural changes in the types.

  def: Parsing allows two kinds of definitions (type annotations or definitions),
       but later checks will see that they are well formed and combine them.

  t: for adding type information, currently unused

  v: for enriching variables with information like provenonce, currently unused

Please don't get on my case to take the unused ones out. I have considered it.
It's a huge pain to add and remove, it will get used eventually, and it's not
hurting anything as it is.
-}
data Expr' def
    = Literal Literal.Literal
    | Var String
    | Range (LExpr' def) (LExpr' def)
    | ExplicitList [LExpr' def]
    | Binop String (LExpr' def) (LExpr' def)
    | Lambda Pattern.Pattern (LExpr' def)
    | App (LExpr' def) (LExpr' def)
    | MultiIf [(LExpr' def,LExpr' def)]
    | Let [def] (LExpr' def)
    | Case (LExpr' def) [(Pattern.Pattern, LExpr' def)]
    | Data String [LExpr' def]
    | Access (LExpr' def) String
    | Remove (LExpr' def) String
    | Insert (LExpr' def) String (LExpr' def)
    | Modify (LExpr' def) [(String, LExpr' def)]
    | Record [(String, LExpr' def)]
    | Markdown String String [LExpr' def]
    -- for type checking and code gen only
    | PortIn String SrcType.Type Type.Variable (LExpr' def)
    | PortOut String SrcType.Type (LExpr' def)

type ParseExpr = Expr' ParseDef
type LParseExpr = LExpr' ParseDef
type LExpr = LExpr' Def
type Expr = Expr' Def

{-| Style of definitions as they are parsed. Users specify them
separately, and other checks will make sure they can be combined.
-}
data ParseDef
    = Def Pattern.Pattern LParseExpr
    | TypeAnnotation String SrcType.Type
      deriving (Show)

{-| After checking that type annotations and definitions are all
valid, they can be combined.
-}
data Def = Definition Pattern.Pattern (LExpr' Def) (Maybe SrcType.Type)
    deriving (Show)

tuple :: [LExpr' def] -> Expr' def
tuple es = Data ("_Tuple" ++ show (length es)) es

delist :: LExpr' def -> [LExpr' def]
delist (Location.L _ (Data "::" [h,t])) = h : delist t
delist _ = []

saveEnvName :: String
saveEnvName = "_save_the_environment!!!"

dummyLet :: Pretty def => [def] -> LExpr' def
dummyLet defs = 
     Location.none $ Let defs (Location.none $ Var saveEnvName)

instance Pretty def => Show (Expr' def) where
  show = render . pretty

instance Pretty def => Pretty (Expr' def) where
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
           pretty' (p,b) = pretty p <+> P.text "->" <+> pretty b
     Data "::" [hd,tl] -> pretty hd <+> P.text "::" <+> pretty tl
     Data "[]" [] -> P.text "[]"
     Data ('_':'T':'u':'p':'l':'e':_num) es -> P.parens (commaCat (map pretty es))
     Data name es -> P.hang (P.text name) 2 (P.sep (map prettyParens es))
     Access e x -> prettyParens e <> P.text "." <> variable x
     Remove e x -> P.braces (pretty e <+> P.text "-" <+> variable x)
     Insert (Location.L _ (Remove e y)) x v ->
         P.braces (pretty e <+> P.text "-" <+> variable y <+> P.text "|" <+> variable x <+> P.equals <+> pretty v)
     Insert e x v ->
         P.braces (pretty e <+> P.text "|" <+> variable x <+> P.equals <+> pretty v)

     Modify e fs ->
         P.braces $ P.hang (pretty e <+> P.text "|")
                           4
                           (commaSep $ map field fs)
       where
         field (k,v) = variable k <+> P.text "<-" <+> pretty v

     Record fs ->
         P.braces $ P.nest 2 (commaSep $ map field fs)
       where
         field (x,e) = variable x <+> P.equals <+> pretty e

     Markdown _ _ _ -> P.text "[markdown| ... |]"

     PortIn _ _ _ handler -> pretty handler

     PortOut _ _ signal -> pretty signal

instance Pretty ParseDef where
  pretty def =
   case def of
     TypeAnnotation name tipe ->
         variable name <+> P.colon <+> pretty tipe
     Def pattern expr ->
         pretty pattern <+> P.equals <+> pretty expr

instance Pretty Def where
  pretty (Definition pattern expr maybeTipe) =
      P.vcat [ annotation, definition ]
      where
        definition = pretty pattern <+> P.equals <+> pretty expr
        annotation = case maybeTipe of
                       Nothing -> P.empty
                       Just tipe -> pretty pattern <+> P.colon <+> pretty tipe

collectApps :: LExpr' def -> [LExpr' def]
collectApps lexpr@(Location.L _ expr) =
  case expr of
    App a b -> collectApps a ++ [b]
    _ -> [lexpr]

collectLambdas :: LExpr' def -> ([Doc], LExpr' def)
collectLambdas lexpr@(Location.L _ expr) =
  case expr of
    Lambda pattern body ->
        let (ps, body') = collectLambdas body
        in  (pretty pattern : ps, body')
    _ -> ([], lexpr)

prettyParens :: (Pretty def) => LExpr' def -> Doc
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
        Data name (_:_) -> name /= "::"
        _ -> False
