{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Expression.Canonical
  ( Expr, Expr'(..)
  , Main(..)
  , Def(..)
  , SortedDefs(..), toSortedDefs
  , localVar
  , collectApps, collectFields, collectLambdas
  )
  where


import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as B

import qualified AST.Effects as Effects
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as Ptrn
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- EXPRESSIONS


type Expr =
    A.Annotated R.Region Expr'


data Expr'
    = Literal Literal.Literal
    | Var Var.Canonical
    | List [Expr]
    | Binop Var.Canonical Expr Expr
    | Lambda Ptrn.Canonical Expr
    | App Expr Expr
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case Expr [(Ptrn.Canonical, Expr)]
    | Ctor Var.Canonical [Expr]
    | Access Expr Text
    | Update Expr [(Text, Expr)]
    | Record [(Text, Expr)]
    -- for type checking and code gen only
    | Cmd ModuleName.Canonical Effects.ManagerType
    | Sub ModuleName.Canonical Effects.ManagerType
    | OutgoingPort Text Type.Canonical
    | IncomingPort Text Type.Canonical
    | Program Main Expr
    | SaveEnv ModuleName.Canonical Effects.Canonical
    | GLShader Text Text Literal.Shader


data Main
  = Static
  | Dynamic Type.Canonical



-- DEFS


data Def =
  Def
    { _region :: R.Region
    , _pattern :: Ptrn.Canonical
    , _body :: Expr
    , _type :: Maybe (A.Located Type.Canonical)
    }



-- SORTED DEFS


data SortedDefs
  = NoMain [Def]
  | YesMain [Def] Def [Def]


toSortedDefs :: Expr -> SortedDefs
toSortedDefs (A.A _ expr) =
  case expr of
    Let defs body ->
      foldr defCons (toSortedDefs body) defs

    _ ->
      NoMain []


defCons :: Def -> SortedDefs -> SortedDefs
defCons def@(Def _ (A.A _ pattern) _ _) sortedDefs =
  case (pattern, sortedDefs) of
    (Ptrn.Var "main", NoMain defs) ->
      YesMain [] def defs

    (_, NoMain defs) ->
      NoMain (def : defs)

    (_, YesMain defs main rest) ->
      YesMain (def : defs) main rest



-- HELPERS


localVar :: Text -> Expr'
localVar x =
  Var (Var.Canonical Var.Local x)


-- COLLECTORS


collectApps :: Expr -> [Expr]
collectApps annExpr@(A.A _ expr) =
  case expr of
    App a b ->
      collectApps a ++ [b]

    _ ->
      [annExpr]


collectFields :: Expr -> Maybe Text
collectFields expr =
  fmap (LText.toStrict . B.toLazyText) (collectFieldsHelp expr mempty)


collectFieldsHelp :: Expr -> B.Builder -> Maybe B.Builder
collectFieldsHelp (A.A _ expr) builder =
  case expr of
    Var var ->
      Just (B.fromText (Var.toText var) <> builder)

    Access record field ->
      collectFieldsHelp record ("." <> B.fromText field <> builder)

    _ ->
      Nothing


collectLambdas :: Expr -> ([Ptrn.Canonical], Expr)
collectLambdas lexpr@(A.A _ expr) =
  case expr of
    Lambda pattern body ->
      let
        (ps, body') = collectLambdas body
      in
        (pattern : ps, body')

    _ ->
      ([], lexpr)
