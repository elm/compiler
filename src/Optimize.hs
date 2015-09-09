module Optimize (optimize) where

import Control.Applicative
import qualified Control.Monad as M
import qualified Data.Traversable as T

import qualified AST.Expression.General as Expr
import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Optimized as Opt
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Optimize.Environment as Env
import qualified Optimize.Patterns as Patterns
import qualified Optimize.Patterns.DecisionTree as DT
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


-- OPTIMIZE

optimize :: DT.VariantDict -> Module.CanonicalModule -> Module.Optimized
optimize variantDict module_@(Module.Module home _ _ _ _ body) =
  let
    (defs, _) =
        flattenLets [] (Module.program body)

    optDefs =
      Env.run variantDict (concat <$> mapM (optimizeDef (Just home)) defs)
  in
    module_ { Module.body = body { Module.program = optDefs } }


flattenLets :: [Can.Def] -> Can.Expr -> ([Can.Def], Can.Expr)
flattenLets defs annExpr@(A.A _ expr) =
    case expr of
      Expr.Let ds body ->
          flattenLets (defs ++ ds) body

      _ ->
          (defs, annExpr)


-- CONVERT DEFINITIONS

optimizeDef :: Maybe ModuleName.Canonical -> Can.Def -> Env.Optimizer [Opt.Def]
optimizeDef home (Can.Definition (Can.Facts deps) pattern@(A.A _ ptrn) expression _) =
  let
    (args, rawBody) =
      Expr.collectLambdas expression
  in
    case (ptrn, args) of
      (P.Var name, _ : _) ->

          do  htc <- Env.getTailCall
              Env.setTailCall False

              (args, body) <- optimizeFunction (Just name) args rawBody

              isTailCall <- Env.getTailCall
              Env.setTailCall htc

              let facts = Opt.Facts home deps

              return $
                if isTailCall then

                  [ Opt.TailDef facts name args body ]

                else

                  [ Opt.Def facts name (Opt.Function args body) ]

      (P.Var name, []) ->

          do  hasTailCall <- Env.getTailCall
              body <- optimizeExpr Nothing rawBody
              Env.setTailCall hasTailCall
              return [ Opt.Def (Opt.Facts home deps) name body ]

      (_, []) ->

          do  name <- Env.freshName
              optBody <- optimizeExpr Nothing rawBody
              let optDef = Opt.Def (Opt.Facts home deps) name optBody
              return (optDef : patternToDefs home name pattern)

      _ ->
          error "there should never be a function where the name is not a P.Var"


-- OPTIMIZE FUNCTION

optimizeFunction
    :: Maybe String
    -> [P.CanonicalPattern]
    -> Can.Expr
    -> Env.Optimizer ([String], Opt.Expr)
optimizeFunction maybeName patterns givenBody =
  do  (argNames, body) <- M.foldM depatternArgs ([], givenBody) (reverse patterns)
      optBody <- optimizeExpr (makeContext argNames =<< maybeName) body
      return (argNames, optBody)


makeContext :: [String] -> String -> Context
makeContext argNames name =
    Just (name, length argNames, argNames)


depatternArgs
    :: ([String], Can.Expr)
    -> P.CanonicalPattern
    -> Env.Optimizer ([String], Can.Expr)
depatternArgs (names, rawExpr) ptrn =
    do  (name, expr) <- depattern ptrn rawExpr
        return (name : names, expr)


patternToDefs
    :: Maybe ModuleName.Canonical
    -> String
    -> P.CanonicalPattern
    -> [Opt.Def]
patternToDefs home rootName pattern =
    let
        (deps, rootExpr) =
            case home of
              Nothing ->
                  ( []
                  , Opt.Var (Var.local rootName)
                  )

              Just moduleName ->
                  ( [ Var.TopLevelVar moduleName rootName ]
                  , Opt.Var (Var.topLevel moduleName rootName)
                  )

        toDef (name, accessExpr) =
            Opt.Def (Opt.Facts home deps) name accessExpr
    in
        map toDef (substitutions rootExpr pattern)


substitutions :: Opt.Expr -> P.CanonicalPattern -> [(String, Opt.Expr)]
substitutions expr (A.A _ pattern) =
  case pattern of
    P.Var name ->
        [ (name, expr) ]

    P.Literal _ ->
        []

    P.Record fields ->
        map (\name -> (name, Opt.Access expr name)) fields

    P.Anything ->
        []

    P.Alias alias realPattern ->
        (alias, expr) : substitutions expr realPattern

    P.Data _ patterns ->
        concat (zipWith (substitutions . Opt.DataAccess expr) [0..] patterns)


-- OPTIMIZE EXPRESSIONS

type Context = Maybe (String, Int, [String])


{-| This optimizer detects tail calls. The `Context` holds a function name,
function arity, and list of argument names. If a tail call is possible, the
`Context` will be filled in and we check any function we run into. If we go
down a route that cannot possibly have tail calls, we remove the context.
-}
optimizeExpr :: Context -> Can.Expr -> Env.Optimizer Opt.Expr
optimizeExpr context annExpr@(A.A region expression) =
  let
    keepLooking =
      optimizeExpr context
      -- this *might* find a tail call, use this when something is a potential
      -- tail call

    justConvert =
      optimizeExpr Nothing
      -- this will never find tail calls, use this when you are working with
      -- expressions that cannot be turned into tail calls
  in
  case expression of
    Expr.Literal lit ->
        pure (Opt.Literal lit)

    Expr.Var name ->
        if name == Var.inCore ["Debug"] "crash" then
            pure (Opt.Crash region Nothing)

        else
            pure (Opt.Var name)

    Expr.Range lowExpr highExpr ->
        Opt.Range
          <$> justConvert lowExpr
          <*> justConvert highExpr

    Expr.ExplicitList elements ->
        Opt.ExplicitList
          <$> T.traverse justConvert elements

    Expr.Binop op leftExpr rightExpr ->
        optimizeBinop context region op leftExpr rightExpr

    Expr.Lambda _ _ ->
        let
            (patterns, body) =
                Expr.collectLambdas annExpr
        in
            uncurry Opt.Function <$> optimizeFunction Nothing patterns body

    Expr.App _ _ ->
        let
            (func:args) =
                Expr.collectApps annExpr
        in
            case isTailCall context func (length args) of
              Just (name, argNames) ->
                  do  optArgs <- T.traverse justConvert args
                      Env.setTailCall True
                      return (Opt.TailCall name argNames optArgs)

              Nothing ->
                  do  hasTailCall <- Env.getTailCall
                      optFunc <- justConvert func
                      optArgs <- T.traverse justConvert args
                      Env.setTailCall hasTailCall
                      return (Opt.Call optFunc optArgs)

    Expr.If branches finally ->
        let
            crawlBranch (cond,branch) =
                (,) <$> justConvert cond <*> keepLooking branch
        in
            Opt.If
              <$> T.traverse crawlBranch branches
              <*> keepLooking finally

    Expr.Let defs body ->
        do  optDefs <- concat <$> T.traverse (optimizeDef Nothing) defs
            Opt.Let optDefs <$> keepLooking body

    Expr.Case expr branches ->
        do  optExpr <- optimizeExpr Nothing expr
            optBranches <- T.traverse (mapSnd (optimizeExpr context)) branches
            variantDict <- Env.getVariantDict
            Patterns.optimize variantDict region optExpr optBranches

    Expr.Data name exprs ->
        Opt.Data name <$> T.traverse justConvert exprs

    Expr.Access record field ->
        Opt.Access
          <$> justConvert record
          <*> pure field

    Expr.Update record fields ->
        Opt.Update
          <$> justConvert record
          <*> T.traverse (mapSnd justConvert) fields

    Expr.Record fields ->
        Opt.Record
          <$> T.traverse (mapSnd justConvert) fields

    Expr.GLShader uid src gltipe ->
        pure (Opt.GLShader uid src gltipe)

    Expr.Port impl ->
        Opt.Port <$>
          case impl of
            Expr.In name tipe ->
                pure (Expr.In name tipe)

            Expr.Out name expr tipe ->
                (\e -> Expr.Out name e tipe) <$> justConvert expr

            Expr.Task name expr tipe ->
                (\e -> Expr.Task name e tipe) <$> justConvert expr


mapSnd :: M.Functor box => (a -> box b) -> (x, a) -> box (x, b)
mapSnd func (x, a) =
  (,) x <$> func a


-- DETECT TAIL CALL

isTailCall :: Context -> Can.Expr -> Int -> Maybe (String, [String])
isTailCall context (A.A _ function) arity =
    case (context, function) of
      ( Just (ctxName, ctxArity, argNames), Expr.Var (Var.Canonical home name) ) ->

          if name == ctxName && arity == ctxArity && Var.isLocalHome home then
              Just (name, argNames)

          else
              Nothing

      _ ->
          Nothing


-- DEPATTERN
-- given a pattern and an expression, push the actual pattern matching into the
-- expression as a case-expression if necessary. The pattern compiler will work
-- it out from there

depattern :: P.CanonicalPattern -> Can.Expr -> Env.Optimizer (String, Can.Expr)
depattern canPattern@(A.A _ pattern) expr =
  let
    ann =
      A.A (error "the annotation added in 'depattern' should not be observed!")

    caseExpr e branches =
      ann (Expr.Case e branches)

    var name =
      ann (Expr.Var (Var.local name))

    caseify =
      do  name <- Env.freshName
          return
            ( name
            , caseExpr (var name) [ (canPattern, expr) ]
            )
  in
  case pattern of
    P.Var name ->
        return (name, expr)

    P.Literal _ ->
        caseify

    P.Record _ ->
        caseify

    P.Anything ->
        do  name <- Env.freshName
            return (name, expr)

    P.Alias _ _ ->
        caseify

    P.Data _ _ ->
        caseify


-- OPTIMIZE BINOPS

optimizeBinop
    :: Context
    -> R.Region
    -> Var.Canonical
    -> Can.Expr
    -> Can.Expr
    -> Env.Optimizer Opt.Expr
optimizeBinop context region op leftExpr rightExpr =
  let
    ann = A.A region
    binop = ann (Expr.Binop op leftExpr rightExpr)
  in
  if op == forwardApply then

      optimizeExpr context (collect id left forwardApply binop)

  else if op == backwardApply then

      optimizeExpr context (collect id right backwardApply binop)

  else if op == forwardCompose then

      do  var <- Env.freshName
          let makeRoot func = ann (Expr.App func (ann (Expr.Var (Var.local var))))
          let body = collect makeRoot left forwardCompose binop
          optimizeExpr context (ann (Expr.Lambda (ann (P.Var var)) body))

  else if op == backwardCompose then

      do  var <- Env.freshName
          let makeRoot func = ann (Expr.App func (ann (Expr.Var (Var.local var))))
          let body = collect makeRoot right backwardCompose binop
          optimizeExpr context (ann (Expr.Lambda (ann (P.Var var)) body))

  else if op == Var.Canonical Var.BuiltIn "::" then

      optimizeExpr context (ann (Expr.Data "::" [ leftExpr, rightExpr ]))

  else

      Opt.Binop op
        <$> optimizeExpr Nothing leftExpr
        <*> optimizeExpr Nothing rightExpr


-- left-associative ((x |> f) |> g)
forwardApply :: Var.Canonical
forwardApply =
  Var.inCore ["Basics"] "|>"


-- right-associative (g <| (f <| x))
backwardApply :: Var.Canonical
backwardApply =
  Var.inCore ["Basics"] "<|"


-- left-associative ((f >> g) >> h)
forwardCompose :: Var.Canonical
forwardCompose =
  Var.inCore ["Basics"] ">>"


-- right-associative (h << (g << f))
backwardCompose :: Var.Canonical
backwardCompose =
  Var.inCore ["Basics"] "<<"


type Assoc =
  Can.Expr -> Can.Expr -> (Can.Expr, Can.Expr)


left :: Assoc
left =
    flip (,)


right :: Assoc
right =
    (,)


collect :: (Can.Expr -> Can.Expr) -> Assoc -> Var.Canonical -> Can.Expr -> Can.Expr
collect makeRoot assoc desiredOp annExpr@(A.A ann expr) =
  case expr of
    Expr.Binop op leftExpr rightExpr | op == desiredOp ->
        let
          (func, arg) = assoc leftExpr rightExpr
        in
          A.A ann (Expr.App func (collect makeRoot assoc desiredOp arg))

    _ ->
        makeRoot annExpr
