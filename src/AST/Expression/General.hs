{-# OPTIONS_GHC -Wall #-}

{-| The Abstract Syntax Tree (AST) for expressions comes in a couple formats.
The first is the fully general version and is labeled with a prime (Expr').
The others are specialized versions of the AST that represent specific phases
of the compilation process. I expect there to be more phases as we begin to
enrich the AST with more information.
-}
module AST.Expression.General where

import AST.PrettyPrint
import Text.PrettyPrint as P
import AST.Type (Type)

import qualified AST.Annotation as Annotation
import qualified AST.Helpers as Help
import qualified AST.Literal as Literal
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var

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
type Expr annotation definition variable =
    Annotation.Annotated annotation (Expr' annotation definition variable)

data Expr' ann def var
    = Literal Literal.Literal
    | Var var
    | Range (Expr ann def var) (Expr ann def var)
    | ExplicitList [Expr ann def var]
    | Binop var (Expr ann def var) (Expr ann def var)
    | Lambda (Pattern.Pattern var) (Expr ann def var)
    | App (Expr ann def var) (Expr ann def var)
    | MultiIf [(Expr ann def var,Expr ann def var)]
    | Let [def] (Expr ann def var)
    | Case (Expr ann def var) [(Pattern.Pattern var, Expr ann def var)]
    | Data String [Expr ann def var]
    | Access (Expr ann def var) String
    | Remove (Expr ann def var) String
    | Insert (Expr ann def var) String (Expr ann def var)
    | Modify (Expr ann def var) [(String, Expr ann def var)]
    | Record [(String, Expr ann def var)]
    -- for type checking and code gen only
    | PortIn String (Type var)
    | PortOut String (Type var) (Expr ann def var)
    | GLShader String String Literal.GLShaderTipe
    deriving (Show)


---- UTILITIES ----

rawVar :: String -> Expr' ann def Var.Raw
rawVar x = Var (Var.Raw x)

localVar :: String -> Expr' ann def Var.Canonical
localVar x = Var (Var.Canonical Var.Local x)

tuple :: [Expr ann def var] -> Expr' ann def var
tuple es = Data ("_Tuple" ++ show (length es)) es

delist :: Expr ann def var -> [Expr ann def var]
delist (Annotation.A _ (Data "::" [h,t])) = h : delist t
delist _ = []

saveEnvName :: String
saveEnvName = "_save_the_environment!!!"

dummyLet :: (Pretty def) => [def] -> Expr Annotation.Region def Var.Canonical
dummyLet defs = 
     Annotation.none $ Let defs (Annotation.none $ Var (Var.builtin saveEnvName))

instance (Pretty def, Pretty var, Var.ToString var) => Pretty (Expr' ann def var) where
  pretty expr =
    case expr of
      Literal lit ->
          pretty lit

      Var x ->
          pretty x

      Range e1 e2 ->
          P.brackets (pretty e1 <> P.text ".." <> pretty e2)

      ExplicitList es ->
          P.brackets (commaCat (map pretty es))

      Binop op (Annotation.A _ (Literal (Literal.IntNum 0))) e
          | Var.toString op == "-" ->
              P.text "-" <> prettyParens e

      Binop op e1 e2 ->
          P.hang (prettyParens e1) 2 (P.text op'' <+> prettyParens e2)
        where
          op' = Var.toString op
          op'' = if Help.isOp op' then op' else "`" ++ op' ++ "`"

      Lambda p e ->
          P.text "\\" <> args <+> P.text "->" <+> pretty body
        where
          (ps,body) = collectLambdas (Annotation.A undefined $ Lambda p e)
          args = P.sep (map Pattern.prettyParens ps)

      App _ _ ->
          P.hang func 2 (P.sep args)
        where
          func:args =
              map prettyParens (collectApps (Annotation.A undefined expr))

      MultiIf branches ->
          P.text "if" $$ nest 3 (vcat $ map iff branches)
        where
          iff (b,e) = P.text "|" <+> P.hang (pretty b <+> P.text "->") 2 (pretty e)

      Let defs e ->
          P.sep
            [ P.hang (P.text "let") 4 (P.vcat (map pretty defs))
            , P.text "in" <+> pretty e
            ]

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

      Access e x ->
          prettyParens e <> P.text "." <> variable x

      Remove e x ->
          P.braces (pretty e <+> P.text "-" <+> variable x)

      Insert (Annotation.A _ (Remove e y)) x v ->
          P.braces $
              P.hsep
                [ pretty e, P.text "-", variable y, P.text "|"
                , variable x, P.equals, pretty v
                ]

      Insert e x v ->
          P.braces (pretty e <+> P.text "|" <+> variable x <+> P.equals <+> pretty v)

      Modify e fs ->
          P.braces $
              P.hang
                  (pretty e <+> P.text "|")
                  4
                  (commaSep $ map field fs)
        where
          field (k,v) = variable k <+> P.text "<-" <+> pretty v

      Record fs ->
          P.sep
            [ P.cat (zipWith (<+>) (P.lbrace : repeat P.comma) (map field fs))
            , P.rbrace
            ]
        where
          field (field, value) =
             variable field <+> P.equals <+> pretty value

      GLShader _ _ _ -> P.text "[glsl| ... |]"

      PortIn name _ -> P.text $ "<port:" ++ name ++ ">"

      PortOut _ _ signal -> pretty signal


collectApps :: Expr ann def var -> [Expr ann def var]
collectApps annExpr@(Annotation.A _ expr) =
  case expr of
    App a b -> collectApps a ++ [b]
    _ -> [annExpr]


collectLambdas :: Expr ann def var -> ([Pattern.Pattern var], Expr ann def var)
collectLambdas lexpr@(Annotation.A _ expr) =
  case expr of
    Lambda pattern body ->
        let (ps, body') = collectLambdas body
        in  (pattern : ps, body')

    _ -> ([], lexpr)


prettyParens :: (Pretty def, Pretty var, Var.ToString var) => Expr ann def var -> Doc
prettyParens (Annotation.A _ expr) =
    parensIf needed (pretty expr)
  where
    needed =
      case expr of
        Binop _ _ _ -> True
        Lambda _ _  -> True
        App _ _     -> True
        MultiIf _   -> True
        Let _ _     -> True
        Case _ _    -> True
        Data name (_:_) -> not (name == "::" || Help.isTuple name)
        _ -> False
