{-# OPTIONS_GHC -Wall #-}

{-| The Abstract Syntax Tree (AST) for expressions comes in a couple formats.
The first is the fully general version and is labeled with a prime (Expr').
The others are specialized versions of the AST that represent specific phases
of the compilation process. I expect there to be more phases as we begin to
enrich the AST with more information.
-}
module AST.Expression.General where

import qualified Data.List as List

import qualified AST.Effects as Effects
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as Pattern
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
    | Cmd ModuleName.Canonical
    | Sub ModuleName.Canonical
    | OutgoingPort String typ
    | IncomingPort String typ
    | Program (Main typ) (Expr ann def var typ)
    | SaveEnv ModuleName.Canonical Effects.Canonical
    | GLShader String String Literal.GLShaderTipe


data Main typ
  = VDom
  | NoFlags
  | Flags typ



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


collectFields :: (Var.ToString var) => Expr ann def var typ -> Maybe String
collectFields expr =
  collectFieldsHelp expr []


collectFieldsHelp :: (Var.ToString var) => Expr ann def var typ -> [String] -> Maybe String
collectFieldsHelp (A.A _ expr) fields =
  case expr of
    Var var ->
      Just (List.intercalate "." (Var.toString var : fields))

    Access record field ->
      collectFieldsHelp record (field : fields)

    _ ->
      Nothing


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
