module Optimize.TailCalls (optimize) where

import Control.Applicative
import qualified Control.Monad as M
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Traversable as T

import qualified AST.Expression.General as Expr
import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Optimized as Opt
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Optimize.Cases as Cases
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


-- OPTIMIZE FOR TAIL CALLS

optimize :: Module.CanonicalModule -> Module.Optimized
optimize module_ =
  let
    home =
      Module.name module_

    (defs, _) =
        flattenLets [] (Module.program (Module.body module_))
  in
    State.evalState
        (concat <$> mapM (definition (Just home)) defs)
        (Env False 0)


flattenLets :: [Can.Def] -> Can.Expr -> ([Can.Def], Can.Expr)
flattenLets defs annExpr@(A.A _ expr) =
    case expr of
      Expr.Let ds body ->
          flattenLets (defs ++ ds) body

      _ ->
          (defs, annExpr)


-- TRACKER TO THREAD FINDINGS THROUGH TRAVERSAL

type Optimizer a =
    State.State Env a


data Env = Env
    { _hasTailCall :: Bool
    , _uid :: Int
    }


setTailCall :: Bool -> Optimizer ()
setTailCall bool =
  do  uid <- State.gets _uid
      State.put (Env bool uid)


mapSnd :: M.Functor box => (a -> box b) -> (x, a) -> box (x, b)
mapSnd func (x, a) =
  (,) x <$> func a


-- CONVERT DEFINITIONS

definition :: Maybe ModuleName.Canonical -> Can.Def -> Optimizer [Opt.Def]
definition home (Can.Definition (Can.Facts deps) pattern@(A.A _ ptrn) expression _) =
  let
    (args, rawBody) =
      Expr.collectLambdas expression
  in
    case (ptrn, args) of
      (P.Var name, _ : _) ->

          do  (Env htc uid) <- State.get
              State.put (Env False uid)

              (args, body) <- optimizeFunction (Just name) args rawBody

              (Env isTailCall uid') <- State.get
              State.put (Env htc uid')

              let facts = Opt.Facts home deps

              return $
                if isTailCall then

                  [ Opt.TailDef facts name args body ]

                else

                  [ Opt.Def facts name (Opt.Function args body) ]

      (P.Var name, []) ->

          do  hasTailCall <- State.gets _hasTailCall
              body <- findTailCalls Nothing rawBody
              setTailCall hasTailCall
              return [ Opt.Def (Opt.Facts home deps) name body ]

      (_, []) ->

          do  name <- freshName
              optBody <- findTailCalls Nothing rawBody
              let optDef = Opt.Def (Opt.Facts home deps) name optBody
              return (optDef : patternToDefs home name pattern)

      _ ->
          error "there should never be a function where the name is not a P.Var"


-- OPTIMIZE FUNCTION

optimizeFunction
    :: Maybe String
    -> [P.CanonicalPattern]
    -> Can.Expr
    -> Optimizer ([String], Opt.Expr)
optimizeFunction maybeName patterns givenBody =
  do  (argNames, body) <- M.foldM depatternArgs ([], givenBody) (reverse patterns)
      optBody <- findTailCalls (makeContext argNames =<< maybeName) body
      return (argNames, optBody)


makeContext :: [String] -> String -> Context
makeContext argNames name =
    Just (name, length argNames, argNames)


depatternArgs
    :: ([String], Can.Expr)
    -> P.CanonicalPattern
    -> Optimizer ([String], Can.Expr)
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


-- CONVERT EXPRESSIONS

type Context = Maybe (String, Int, [String])


findTailCalls :: Context -> Can.Expr -> Optimizer Opt.Expr
findTailCalls context annExpr@(A.A region expression) =
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
  case expression of
    Expr.Literal lit ->
        pure (Opt.Literal lit)

    Expr.Var name ->
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
                      setTailCall True
                      return (Opt.TailCall name argNames optArgs)

              Nothing ->
                  do  hasTailCall <- State.gets _hasTailCall
                      optFunc <- justConvert func
                      optArgs <- T.traverse justConvert args
                      setTailCall hasTailCall
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
        do  optDefs <- concat <$> T.traverse (definition Nothing) defs
            Opt.Let optDefs <$> keepLooking body

    Expr.Case expr rawBranches ->
        let
            indexify index (pattern, branch) =
                ( (pattern, index)
                , (index, branch)
                )

            (patterns, indexedBranches) =
                unzip (zipWith indexify [0..] rawBranches)

            decisionTree =
                Cases.compile (error "need a VariantDict") patterns

            branches =
                Map.fromList indexedBranches
        in
            Opt.Case
              <$> justConvert expr
              <*> pure decisionTree
              <*> T.traverse keepLooking branches

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

depattern :: P.CanonicalPattern -> Can.Expr -> Optimizer (String, Can.Expr)
depattern canPattern@(A.A _ pattern) expr =
  let
    ann =
      A.A (error "the annotation added in 'depattern' should not be observed!")

    caseExpr e branches =
      ann (Expr.Case e branches)

    var name =
      ann (Expr.Var (Var.local name))

    caseify =
      do  name <- freshName
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
        do  name <- freshName
            return (name, expr)

    P.Alias _ _ ->
        caseify

    P.Data _ _ ->
        caseify


freshName :: Optimizer String
freshName =
  do  (Env htc uid) <- State.get
      State.put (Env htc (uid + 1))
      return ("_p" ++ show uid)


-- OPTIMIZE BINOPS

optimizeBinop
    :: Context
    -> R.Region
    -> Var.Canonical
    -> Can.Expr
    -> Can.Expr
    -> Optimizer Opt.Expr
optimizeBinop context region op leftExpr rightExpr =
  let
    ann = A.A region
    binop = ann (Expr.Binop op leftExpr rightExpr)
  in
  if op == forwardApply then

      findTailCalls context (collect id left forwardApply binop)

  else if op == backwardApply then

      findTailCalls context (collect id right backwardApply binop)

  else if op == forwardCompose then

      do  var <- freshName
          let makeRoot func = ann (Expr.App func (ann (Expr.Var (Var.local var))))
          let body = collect makeRoot left forwardCompose binop
          findTailCalls context (ann (Expr.Lambda (ann (P.Var var)) body))

  else if op == backwardCompose then

      do  var <- freshName
          let makeRoot func = ann (Expr.App func (ann (Expr.Var (Var.local var))))
          let body = collect makeRoot right backwardCompose binop
          findTailCalls context (ann (Expr.Lambda (ann (P.Var var)) body))

  else if op == Var.Canonical Var.BuiltIn "::" then

      findTailCalls context (ann (Expr.Data "::" [ leftExpr, rightExpr ]))

  else

      Opt.Binop op
        <$> findTailCalls Nothing leftExpr
        <*> findTailCalls Nothing rightExpr


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
