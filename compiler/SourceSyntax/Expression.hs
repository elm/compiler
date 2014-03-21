{-# OPTIONS_GHC -Wall #-}

{-| The Abstract Syntax Tree (AST) for expressions comes in a couple formats.
The first is the fully general version and is labeled with a prime (Expr').
The others are specialized versions of the AST that represent specific phases
of the compilation process. I expect there to be more phases as we begin to
enrich the AST with more information.
-}
module SourceSyntax.Expression where

import SourceSyntax.PrettyPrint
import Text.PrettyPrint as P
import qualified SourceSyntax.Helpers as Help
import qualified SourceSyntax.Annotation as Annotation
import qualified SourceSyntax.Pattern as Pattern
import qualified SourceSyntax.Type as SrcType
import qualified SourceSyntax.Literal as Literal
import qualified SourceSyntax.Variable as Variable

---- GENERAL AST ----

{-| This is a fully general Abstract Syntax Tree (AST) for expressions. It has
"type holes" that allow us to enrich the AST with additional information as we
move through the compilation process. The type holes are used to represent:

  ann: Annotations for arbitrary expressions. Allows you to add information
       to the AST like position in source code or inferred types.

  def: Definition style. The source syntax separates type annotations and
       definitions, but after parsing we check that they are well formed and
       collapse them.

  var: Representation of variables. Starts as strings, but is later enriched
       with information about what module a variable came from.

-}
type GeneralExpr annotation definition variable =
    Annotation.Annotated annotation (GeneralExpr' annotation definition variable)

data GeneralExpr' ann def var
    = Literal Literal.Literal
    | Var var
    | Range (GeneralExpr ann def var) (GeneralExpr ann def var)
    | ExplicitList [GeneralExpr ann def var]
    | Binop String (GeneralExpr ann def var) (GeneralExpr ann def var)
    | Lambda Pattern.Pattern (GeneralExpr ann def var)
    | App (GeneralExpr ann def var) (GeneralExpr ann def var)
    | MultiIf [(GeneralExpr ann def var,GeneralExpr ann def var)]
    | Let [def] (GeneralExpr ann def var)
    | Case (GeneralExpr ann def var) [(Pattern.Pattern, GeneralExpr ann def var)]
    | Data String [GeneralExpr ann def var]
    | Access (GeneralExpr ann def var) String
    | Remove (GeneralExpr ann def var) String
    | Insert (GeneralExpr ann def var) String (GeneralExpr ann def var)
    | Modify (GeneralExpr ann def var) [(String, GeneralExpr ann def var)]
    | Record [(String, GeneralExpr ann def var)]
    | Markdown String String [GeneralExpr ann def var]
    -- for type checking and code gen only
    | PortIn String SrcType.Type
    | PortOut String SrcType.Type (GeneralExpr ann def var)
    deriving (Show)


---- SPECIALIZED ASTs ----

{-| Expressions created by the parser. These use a split representation of type
annotations and definitions, which is how they appear in source code and how
they are parsed.
-}
type ParseExpr = GeneralExpr Annotation.Region ParseDef Variable.Raw
type ParseExpr' = GeneralExpr' Annotation.Region ParseDef Variable.Raw

data ParseDef
    = Def Pattern.Pattern ParseExpr
    | TypeAnnotation String SrcType.Type
    deriving (Show)

{-| "Normal" expressions. When the compiler checks that type annotations and
ports are all paired with definitions in the appropriate order, it collapses
them into a Def that is easier to work with in later phases of compilation.
-}
type Expr = GeneralExpr Annotation.Region Def Variable.Raw
type Expr' = GeneralExpr' Annotation.Region Def Variable.Raw

data Def = Definition Pattern.Pattern Expr (Maybe SrcType.Type)
    deriving (Show)



---- UTILITIES ----

rawVar :: String -> GeneralExpr' ann def Variable.Raw
rawVar x = Var (Variable.Raw x)

tuple :: [GeneralExpr ann def var] -> GeneralExpr' ann def var
tuple es = Data ("_Tuple" ++ show (length es)) es

delist :: GeneralExpr ann def var -> [GeneralExpr ann def var]
delist (Annotation.A _ (Data "::" [h,t])) = h : delist t
delist _ = []

saveEnvName :: String
saveEnvName = "_save_the_environment!!!"

dummyLet :: (Pretty def) => [def] -> GeneralExpr Annotation.Region def Variable.Raw
dummyLet defs = 
     Annotation.none $ Let defs (Annotation.none $ rawVar saveEnvName)

instance (Pretty def, Pretty var) => Pretty (GeneralExpr' ann def var) where
  pretty expr =
   case expr of
     Literal lit -> pretty lit

     Var x -> pretty x

     Range e1 e2 -> P.brackets (pretty e1 <> P.text ".." <> pretty e2)

     ExplicitList es -> P.brackets (commaCat (map pretty es))

     Binop "-" (Annotation.A _ (Literal (Literal.IntNum 0))) e ->
         P.text "-" <> prettyParens e

     Binop op e1 e2 -> P.sep [ prettyParens e1 <+> P.text op', prettyParens e2 ]
         where
           op' = if Help.isOp op then op else "`" ++ op ++ "`"

     Lambda p e -> P.text "\\" <> args <+> P.text "->" <+> pretty body
         where
           (ps,body) = collectLambdas (Annotation.A undefined $ Lambda p e)
           args = P.sep (map Pattern.prettyParens ps)

     App _ _ -> P.hang func 2 (P.sep args)
         where
           func:args = map prettyParens (collectApps (Annotation.A undefined expr))

     MultiIf branches -> P.text "if" $$ nest 3 (vcat $ map iff branches)
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

     Insert (Annotation.A _ (Remove e y)) x v ->
         P.braces $ P.hsep [ pretty e, P.text "-", variable y, P.text "|"
                           , variable x, P.equals, pretty v ]

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

collectApps :: GeneralExpr ann def var -> [GeneralExpr ann def var]
collectApps annExpr@(Annotation.A _ expr) =
  case expr of
    App a b -> collectApps a ++ [b]
    _ -> [annExpr]

collectLambdas :: GeneralExpr ann def var -> ([Pattern.Pattern], GeneralExpr ann def var)
collectLambdas lexpr@(Annotation.A _ expr) =
  case expr of
    Lambda pattern body ->
        let (ps, body') = collectLambdas body
        in  (pattern : ps, body')

    _ -> ([], lexpr)

prettyParens :: (Pretty def, Pretty var) => GeneralExpr ann def var -> Doc
prettyParens (Annotation.A _ expr) = parensIf needed (pretty expr)
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
