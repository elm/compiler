{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Source
  ( RawExpr, RawExpr', RawDef, RawDef'(..)
  , ValidExpr, ValidExpr', ValidDef(..)
  , Expr, Expr'(..)
  , var, tuple, zero, getPattern, collectLambdas
  )
  where

import Data.Text (Text)

import qualified AST.Helpers as Help
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
    | Binop [(Expr def, A.Located Text)] (Expr def)
    | Lambda Ptrn.Raw (Expr def)
    | App (Expr def) (Expr def)
    | If [(Expr def, Expr def)] (Expr def)
    | Let [def] (Expr def)
    | Case (Expr def) [(Ptrn.Raw, Expr def)]
    | Ctor Text [Expr def]
    | Access (Expr def) Text
    | Update (Expr def) [(A.Located Text, Expr def)]
    | Record [(A.Located Text, Expr def)]
    | GLShader Text Text Literal.Shader


type RawExpr = Expr RawDef

type RawExpr' = Expr' RawDef

type ValidExpr = Expr ValidDef

type ValidExpr' = Expr' ValidDef



-- DEFINITIONS


type RawDef =
    A.Located RawDef'


data RawDef'
    = Definition Ptrn.Raw RawExpr
    | Annotation Text Type.Raw


data ValidDef =
    Def R.Region Ptrn.Raw ValidExpr (Maybe Type.Raw)



-- HELPERS


var :: Text -> Expr' def
var x =
  Var (Var.Raw x)


tuple :: [Expr def] -> Expr' def
tuple expressions =
  Ctor (Help.makeTuple (length expressions)) expressions


zero :: Expr' def
zero =
  Literal (Literal.IntNum 0)


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
