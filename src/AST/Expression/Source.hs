{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Source
  ( RawExpr, RawExpr', RawDef, RawDef'(..)
  , ValidExpr, ValidExpr', ValidDef(..)
  , Expr, Expr'(..)
  , zero, collectLambdas
  )
  where


import Data.Text (Text)

import qualified AST.Literal as Literal
import qualified AST.Pattern as Ptrn
import qualified AST.Type as Type
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr def =
    A.Located (Expr' def)


data Expr' def
    = Literal Literal.Literal
    | Var (Maybe Text) Text
    | List [Expr def]
    | Op Text
    | Binop [(Expr def, A.Located Text)] (Expr def)
    | Lambda Ptrn.Raw (Expr def)
    | App (Expr def) (Expr def)
    | If [(Expr def, Expr def)] (Expr def)
    | Let [def] (Expr def)
    | Case (Expr def) [(Ptrn.Raw, Expr def)]
    | Accessor Text
    | Access (Expr def) Text
    | Update (A.Located Text) [(A.Located Text, Expr def)]
    | Record [(A.Located Text, Expr def)]
    | Unit
    | Tuple (Expr def) (Expr def) [Expr def]
    | GLShader Text Text Literal.Shader


type RawExpr = Expr RawDef

type RawExpr' = Expr' RawDef

type ValidExpr = Expr ValidDef

type ValidExpr' = Expr' ValidDef



-- DEFINITIONS


type RawDef =
    A.Located RawDef'


data RawDef'
    = Annotation Text Type.Raw
    | Definition Text [Ptrn.Raw] RawExpr
    | Destructure Ptrn.Raw RawExpr


data ValidDef
    = VDefinition R.Region Text [Ptrn.Raw] ValidExpr (Maybe Type.Raw)
    | VDestructure R.Region Ptrn.Raw ValidExpr



-- HELPERS


zero :: Expr' def
zero =
  Literal (Literal.IntNum 0)


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
