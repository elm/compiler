{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Source
  ( RawExpr, RawExpr', RawDef, RawDef'(..)
  , ValidExpr, ValidExpr', ValidDef(..)
  , Expr, Expr'(..)
  , var, tuple, getPattern, collectLambdas
  )
  where


import qualified AST.Literal as Literal
import qualified AST.Pattern as Ptrn
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr def =
    A.Located (Expr' def)


data Expr' def
    = Literal Literal.Literal
    | Var Var.Raw
    | List [Expr def]
    | Binop [(Expr def, A.Located String)] (Expr def)
    | Lambda Ptrn.Raw (Expr def)
    | App (Expr def) (Expr def)
    | If [(Expr def, Expr def)] (Expr def)
    | Let [def] (Expr def)
    | Case (Expr def) [(Ptrn.Raw, Expr def)]
    | Ctor String [Expr def]
    | Access (Expr def) String
    | Update (Expr def) [(A.Located String, Expr def)]
    | Record [(A.Located String, Expr def)]
    | GLShader String String Literal.GLShaderTipe


type RawExpr = Expr RawDef

type RawExpr' = Expr' RawDef

type ValidExpr = Expr ValidDef

type ValidExpr' = Expr' ValidDef



-- DEFINITIONS


type RawDef =
    A.Located RawDef'


data RawDef'
    = Definition Ptrn.Raw RawExpr
    | Annotation String Type.Raw


data ValidDef =
    Def R.Region Ptrn.Raw ValidExpr (Maybe Type.Raw)



-- HELPERS


var :: String -> Expr' def
var x =
  Var (Var.Raw x)


tuple :: [Expr def] -> Expr' def
tuple expressions =
  Ctor ("_Tuple" ++ show (length expressions)) expressions


getPattern :: ValidDef -> Ptrn.Raw
getPattern (Def _ pattern _ _) =
  pattern


collectLambdas :: Expr def -> ([Ptrn.Raw], Expr def)
collectLambdas lexpr@(A.A _ expr) =
  case expr of
    Lambda pattern body ->
      let
        (ps, body') = collectLambdas body
      in
        (pattern : ps, body')

    _ ->
      ([], lexpr)
