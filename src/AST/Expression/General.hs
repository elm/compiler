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

import qualified AST.Annotation as Annotation
import qualified AST.Helpers as Help
import qualified AST.Literal as Literal
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
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
    | Port (PortImpl (Expr ann def var) var)
    | GLShader String String Literal.GLShaderTipe
    deriving (Show)


-- PORTS

data PortImpl expr var
    = In String (Type.PortType var)
    | Out String expr (Type.PortType var)
    | Task String expr (Type.PortType var)
    deriving (Show)


portName :: PortImpl expr var -> String
portName impl =
  case impl of
    In name _ -> name
    Out name _ _ -> name
    Task name _ _ -> name


---- UTILITIES ----

rawVar :: String -> Expr' ann def Var.Raw
rawVar x =
  Var (Var.Raw x)


localVar :: String -> Expr' ann def Var.Canonical
localVar x =
  Var (Var.Canonical Var.Local x)


tuple :: [Expr ann def var] -> Expr' ann def var
tuple expressions =
  Data ("_Tuple" ++ show (length expressions)) expressions


saveEnvName :: String
saveEnvName =
  "_save_the_environment!!!"


dummyLet :: (Pretty def) => [def] -> Expr Annotation.Region def Var.Canonical
dummyLet defs =
  Annotation.none $ Let defs (Annotation.none $ Var (Var.builtin saveEnvName))


instance (Pretty def, Pretty var, Var.ToString var) => Pretty (Expr' ann def var) where
  pretty expression =
    case expression of
      Literal literal ->
          pretty literal

      Var x ->
          pretty x

      Range lowExpr highExpr ->
          P.brackets (pretty lowExpr <> P.text ".." <> pretty highExpr)

      ExplicitList elements ->
          P.brackets (commaCat (map pretty elements))

      Binop op (Annotation.A _ (Literal (Literal.IntNum 0))) expr
          | Var.toString op == "-" ->
              P.text "-" <> prettyParens expr

      Binop op leftExpr rightExpr ->
          P.hang (prettyParens leftExpr) 2 (P.text op'' <+> prettyParens rightExpr)
        where
          op' = Var.toString op
          op'' = if Help.isOp op' then op' else "`" ++ op' ++ "`"

      Lambda pattern expr ->
          P.text "\\" <> args <+> P.text "->" <+> pretty body
        where
          (patterns, body) = collectLambdas expr
          args = P.sep (map Pattern.prettyParens (pattern : patterns))

      App expr arg ->
          P.hang func 2 (P.sep args)
        where
          func:args =
              map prettyParens (collectApps expr ++ [arg])

      MultiIf branches ->
          P.text "if" $$ nest 3 (vcat $ map iff branches)
        where
          iff (b,e) = P.text "|" <+> P.hang (pretty b <+> P.text "->") 2 (pretty e)

      Let defs body ->
          P.sep
            [ P.hang (P.text "let") 4 (P.vcat (map pretty defs))
            , P.text "in" <+> pretty body
            ]

      Case expr branches ->
          P.hang pexpr 2 (P.vcat (map pretty' branches))
        where
          pexpr = P.sep [ P.text "case" <+> pretty expr, P.text "of" ]
          pretty' (pattern, branch) =
              pretty pattern <+> P.text "->" <+> pretty branch

      Data "::" [hd,tl] ->
          pretty hd <+> P.text "::" <+> pretty tl

      Data "[]" [] ->
          P.text "[]"

      Data name exprs
        | Help.isTuple name ->
            P.parens (commaCat (map pretty exprs))
        | otherwise ->
            P.hang (P.text name) 2 (P.sep (map prettyParens exprs))

      Access record field ->
          prettyParens record <> P.text "." <> variable field

      Remove record field ->
          P.braces (pretty record <+> P.text "-" <+> variable field)

      Insert (Annotation.A _ (Remove record y)) x v ->
          P.braces $
              P.hsep
                [ pretty record, P.text "-", variable y, P.text "|"
                , variable x, P.equals, pretty v
                ]

      Insert record field expr ->
          P.braces (pretty record <+> P.text "|" <+> variable field <+> P.equals <+> pretty expr)

      Modify record fields ->
          P.braces $
              P.hang
                  (pretty record <+> P.text "|")
                  4
                  (commaSep $ map field fields)
        where
          field (k,v) = variable k <+> P.text "<-" <+> pretty v

      Record fields ->
          P.sep
            [ P.cat (zipWith (<+>) (P.lbrace : repeat P.comma) (map field fields))
            , P.rbrace
            ]
        where
          field (name, expr) =
             variable name <+> P.equals <+> pretty expr

      GLShader _ _ _ ->
          P.text "[glsl| ... |]"

      Port portImpl ->
          pretty portImpl


instance (Pretty expr, Pretty var) => Pretty (PortImpl expr var) where
  pretty impl =
      P.text ("<port:" ++ portName impl ++ ">")


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
