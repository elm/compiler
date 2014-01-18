{-# OPTIONS_GHC -Wall #-}
module SourceSyntax.Expression where
{-| The Abstract Syntax Tree (AST) for expressions comes in a couple formats.
The first is the fully general version and is labeled with a prime (Expr').
The others are specialized versions of the AST that represent specific phases
of the compilation process. I expect there to be more phases as we begin to
enrich the AST with more information.
-}


import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P
import qualified SourceSyntax.Helpers as Help
import qualified SourceSyntax.Location as Location
import qualified SourceSyntax.Pattern as Pattern
import qualified SourceSyntax.Type as SrcType
import qualified SourceSyntax.Literal as Literal

---- GENERAL AST ----

{-| This is a located expression, meaning it is tagged with info about where it
came from in the source code. Expr' is defined in terms of LExpr' so that the
location information does not need to be an extra field on every constructor.
-}
type LExpr' def = Location.Located (Expr' def)

{-| This is a fully general Abstract Syntax Tree (AST) for expressions. It has
"type holes" that allow us to enrich the AST with additional information as we
move through the compilation process. The type holes let us show these
structural changes in the types. The only type hole right now is:

  def: Parsing allows two kinds of definitions (type annotations or definitions),
       but later checks will see that they are well formed and combine them.

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
    | PortIn String SrcType.Type
    | PortOut String SrcType.Type (LExpr' def)


---- SPECIALIZED ASTs ----

{-| Expressions created by the parser. These use a split representation of type
annotations and definitions, which is how they appear in source code and how
they are parsed.
-}
type ParseExpr = Expr' ParseDef
type LParseExpr = LExpr' ParseDef

data ParseDef
    = Def Pattern.Pattern LParseExpr
    | TypeAnnotation String SrcType.Type
      deriving (Show)

{-| "Normal" expressions. When the compiler checks that type annotations and
ports are all paired with definitions in the appropriate order, it collapses
them into a Def that is easier to work with in later phases of compilation.
-}
type LExpr = LExpr' Def
type Expr = Expr' Def

data Def = Definition Pattern.Pattern LExpr (Maybe SrcType.Type)
    deriving (Show)


---- UTILITIES ----

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
     Lambda p e -> P.text "\\" <> args <+> P.text "->" <+> pretty body
         where
           (ps,body) = collectLambdas (Location.none $ Lambda p e)
           args = P.sep (map Pattern.prettyParens ps)
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
     Data name es
         | Help.isTuple name -> P.parens (commaCat (map pretty es))
         | otherwise -> P.hang (P.text name) 2 (P.sep (map prettyParens es))
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

     PortIn name _ -> P.text $ "<port:" ++ name ++ ">"

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

collectLambdas :: LExpr' def -> ([Pattern.Pattern], LExpr' def)
collectLambdas lexpr@(Location.L _ expr) =
  case expr of
    Lambda pattern body -> (pattern : ps, body')
        where (ps, body') = collectLambdas body
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
