{-# OPTIONS_GHC -Wall #-}

{-| The Abstract Syntax Tree (AST) for expressions comes in a couple formats.
The first is the fully general version and is labeled with a prime (Expr').
The others are specialized versions of the AST that represent specific phases
of the compilation process. I expect there to be more phases as we begin to
enrich the AST with more information.
-}
module AST.Expression.General where

import qualified AST.Literal as Literal
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A



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
type Expr annotation definition variable tipe =
    A.Annotated annotation (Expr' annotation definition variable tipe)


data Expr' ann def var typ
    = Literal Literal.Literal
    | Var var
    | Range (Expr ann def var typ) (Expr ann def var typ)
    | ExplicitList [Expr ann def var typ]
    | Binop var (Expr ann def var typ) (Expr ann def var typ)
    | Lambda (Pattern.Pattern ann var) (Expr ann def var typ)
    | App (Expr ann def var typ) (Expr ann def var typ)
    | If [(Expr ann def var typ, Expr ann def var typ)] (Expr ann def var typ)
    | Let [def] (Expr ann def var typ)
    | Case (Expr ann def var typ) [(Pattern.Pattern ann var, Expr ann def var typ)]
    | Data String [Expr ann def var typ]
    | Access (Expr ann def var typ) String
    | Update (Expr ann def var typ) [(String, Expr ann def var typ)]
    | Record [(String, Expr ann def var typ)]
    -- for type checking and code gen only
    | Port (PortImpl (Expr ann def var typ) typ)
    | GLShader String String Literal.GLShaderTipe



-- PORTS


data PortImpl expr tipe
    = In String (Type.Port tipe)
    | Out String expr (Type.Port tipe)
    | Task String expr (Type.Port tipe)
    deriving (Eq)


portName :: PortImpl expr tipe -> String
portName impl =
  case impl of
    In name _ -> name
    Out name _ _ -> name
    Task name _ _ -> name



---- UTILITIES ----


rawVar :: String -> Expr' ann def Var.Raw typ
rawVar x =
  Var (Var.Raw x)


localVar :: String -> Expr' ann def Var.Canonical typ
localVar x =
  Var (Var.Canonical Var.Local x)


tuple :: [Expr ann def var typ] -> Expr' ann def var typ
tuple expressions =
  Data ("_Tuple" ++ show (length expressions)) expressions


saveEnvName :: String
saveEnvName =
  "_save_the_environment!!!"


dummyLet :: [def] -> Expr ann def Var.Canonical typ
dummyLet defs =
  let
    body =
      A.A undefined (Var (Var.builtin saveEnvName))
  in
    A.A undefined (Let defs body)


collectApps :: Expr ann def var typ -> [Expr ann def var typ]
collectApps annExpr@(A.A _ expr) =
  case expr of
    App a b ->
      collectApps a ++ [b]

    _ ->
      [annExpr]


collectLambdas :: Expr ann def var typ -> ([Pattern.Pattern ann var], Expr ann def var typ)
collectLambdas lexpr@(A.A _ expr) =
  case expr of
    Lambda pattern body ->
      let
        (ps, body') = collectLambdas body
      in
        (pattern : ps, body')

    _ ->
      ([], lexpr)
