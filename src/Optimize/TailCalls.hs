module Optimize.TailCalls where

import Control.Applicative
import Control.Arrow (first)
import qualified Control.Monad as M
import qualified Data.List as List
import qualified Data.Traversable as T

import AST.Expression.General as Expr
import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Optimized as Opt
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


-- OPTIMIZE FOR TAIL CALLS

optimize :: Can.Expr -> Opt.Expr
optimize expression =
    let
        (Result _ optExpr) =
            findTailCalls Nothing expression
    in
        optExpr


-- TRACKER TO THREAD FINDINGS THROUGH TRAVERSAL

data Result a = Result Bool a


instance M.Functor Result where
    fmap func (Result hasTailCall value) =
        Result hasTailCall (func value)


instance Applicative Result where
    pure value =
        Result False value

    (<*>) (Result hasTailCall func) (Result hasTailCall' value) =
        Result (hasTailCall || hasTailCall') (func value)


mapSnd :: M.Functor box => (a -> box b) -> (x, a) -> box (x, b)
mapSnd func (x, a) =
  (,) x <$> func a


-- CONVERT DEFINITIONS

detectTailRecursion :: Can.Def -> Opt.Def
detectTailRecursion (Can.Definition pattern expression _) =
    let
        (args, body) =
            Expr.collectLambdas expression

        context :: Context
        context =
            case (pattern, args) of
              -- detect if it is a function
              (A.A _ (P.Var name), _ : _) ->
                  Just (name, length args)

              _ ->
                  Nothing

        (Result isTailCall optExpr) =
            findTailCalls context body

        lambda x e =
            A.A () (Lambda x e)
    in
        Opt.Definition
          (Opt.Facts (if isTailCall then fmap fst context else Nothing))
          (removeAnnotations pattern)
          (foldr lambda optExpr (map removeAnnotations args))


-- CONVERT EXPRESSIONS

type Context = Maybe (String, Int)


findTailCalls :: Context -> Can.Expr -> Result Opt.Expr
findTailCalls context annExpr@(A.A _ expression) =
  let
    keepLooking =
      findTailCalls context
      -- this *might* find a tail call, use this when something is a potential
      -- tail call

    justConvert =
      findTailCalls Nothing
      -- this will never find tail calls, use this when you are working with
      -- expressions that cannot be turned into tail calls
  in
  A.A () <$>
  case expression of
    Literal lit ->
        pure (Literal lit)

    Var name ->
        pure (Var name)

    Range lowExpr highExpr ->
        Range
          <$> justConvert lowExpr
          <*> justConvert highExpr

    ExplicitList elements ->
        ExplicitList
          <$> T.traverse justConvert elements

    Binop op leftExpr rightExpr ->
        Binop op
          <$> justConvert leftExpr
          <*> justConvert rightExpr

    Lambda pattern body ->
        Lambda (removeAnnotations pattern) <$> justConvert body

    App _ _ ->
        let
            (func:args) =
                Expr.collectApps annExpr

            isTailCall =
                case (context, func) of
                  ( Just (name, argNum), A.A _ (Var (Var.Canonical Var.Local name')) ) ->
                      name == name' && length args == argNum
                  _ ->
                      False

            (Result _ (A.A _ optFunc)) =
                justConvert func

            (Result _ optArgs) =
                T.traverse justConvert args

            apply f arg =
                App (A.A () f) arg
        in
            Result isTailCall (List.foldl' apply optFunc optArgs)


    MultiIf branches finally ->
        let
            crawlBranch (cond,branch) =
                (,) <$> justConvert cond <*> keepLooking branch
        in
            MultiIf
              <$> T.traverse crawlBranch branches
              <*> keepLooking finally

    Let defs body ->
        let
            optDefs = map detectTailRecursion defs
        in
            Let optDefs <$> keepLooking body

    Case expr cases ->
        let
            preppedCases =
                map (first removeAnnotations) cases
        in
            Case
              <$> justConvert expr
              <*> T.traverse (mapSnd keepLooking) preppedCases

    Data name exprs ->
        Data name <$> T.traverse justConvert exprs

    Access record field ->
        Access
          <$> justConvert record
          <*> pure field

    Remove record field ->
        Remove
          <$> justConvert record
          <*> pure field

    Insert record field expr ->
        Insert
          <$> justConvert record
          <*> pure field
          <*> justConvert expr

    Modify record fields ->
        Modify
          <$> justConvert record
          <*> T.traverse (mapSnd justConvert) fields

    Record fields ->
        Record
          <$> T.traverse (mapSnd justConvert) fields

    GLShader uid src gltipe ->
        pure (GLShader uid src gltipe)

    Port impl ->
        Port <$>
          case impl of
            In name tipe ->
                pure (In name tipe)

            Out name expr tipe ->
                (\e -> Out name e tipe) <$> justConvert expr

            Task name expr tipe ->
                (\e -> Task name e tipe) <$> justConvert expr

    Crash details ->
        pure (Crash details)


removeAnnotations :: P.CanonicalPattern -> P.Optimized
removeAnnotations (A.A _ pattern) =
  A.A () $
    case pattern of
      P.Var x ->
          P.Var x

      P.Literal lit ->
          P.Literal lit

      P.Record fields ->
          P.Record fields

      P.Anything ->
          P.Anything

      P.Alias alias realPattern ->
          P.Alias alias (removeAnnotations realPattern)

      P.Data name patterns ->
          P.Data name (map removeAnnotations patterns)
