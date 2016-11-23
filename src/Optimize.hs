module Optimize (optimize) where

import qualified Control.Monad as M
import qualified Data.Map as Map
import qualified Data.Traversable as T

import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Optimize.Case as Case
import qualified Optimize.DecisionTree as DT
import qualified Optimize.Environment as Env
import qualified Optimize.Inline as Inline
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- OPTIMIZE


optimize :: DT.VariantDict -> ModuleName.Canonical -> [Can.Def] -> [Opt.Def]
optimize variantDict home defs =
  Env.run variantDict home (concat <$> mapM (optimizeDef True) defs)



-- CONVERT DEFINITIONS


optimizeDef :: Bool -> Can.Def -> Env.Optimizer [Opt.Def]
optimizeDef isRoot (Can.Def _ pattern expression _) =
  let
    (args, canBody) =
      Can.collectLambdas expression

    maybeGetHome =
      if isRoot then
        Just <$> Env.getHome

      else
        return Nothing
  in
    do  home <- maybeGetHome
        optimizeDefHelp home pattern args canBody


optimizeDefHelp
    :: Maybe ModuleName.Canonical
    -> P.Canonical
    -> [P.Canonical]
    -> Can.Expr
    -> Env.Optimizer [Opt.Def]
optimizeDefHelp home pattern@(A.A _ ptrn) args rawBody =
  let
    facts =
      Opt.Facts home
  in
  case (ptrn, args) of
    (P.Var name, _ : _) ->

        do  htc <- Env.getTailCall
            Env.setTailCall False

            (args, body) <- optimizeFunction (Just name) args rawBody

            isTailCall <- Env.getTailCall
            Env.setTailCall htc

            return $
              if isTailCall then
                  [ Opt.TailDef facts name args body ]

              else
                  [ Opt.Def facts name (Opt.Function args body) ]

    (P.Var name, []) ->

        do  hasTailCall <- Env.getTailCall
            body <- optimizeExpr Nothing rawBody
            Env.setTailCall hasTailCall
            return [ Opt.Def facts name body ]

    (_, []) ->

        do  name <- Env.freshName
            optBody <- optimizeExpr Nothing rawBody
            let optDef = Opt.Def facts name optBody
            return (optDef : patternToDefs home name pattern)

    _ ->
        error "there should never be a function where the name is not a P.Var"



-- OPTIMIZE FUNCTION


optimizeFunction
    :: Maybe String
    -> [P.Canonical]
    -> Can.Expr
    -> Env.Optimizer ([String], Opt.Expr)
optimizeFunction maybeName patterns givenBody =
  do  (argNames, body) <- M.foldM depatternArgs ([], givenBody) (reverse patterns)
      optBody <- optimizeExpr (makeContext argNames =<< maybeName) body
      return (argNames, optBody)


makeContext :: [String] -> String -> Context
makeContext argNames name =
    Just (name, length argNames, argNames)


depatternArgs :: ([String], Can.Expr) -> P.Canonical -> Env.Optimizer ([String], Can.Expr)
depatternArgs (names, rawExpr) ptrn =
    do  (name, expr) <- depattern ptrn rawExpr
        return (name : names, expr)


patternToDefs :: Maybe ModuleName.Canonical -> String -> P.Canonical -> [Opt.Def]
patternToDefs home rootName pattern =
    let
        rootExpr =
            case home of
              Nothing ->
                  Opt.Var (Var.local rootName)

              Just moduleName ->
                  Opt.Var (Var.topLevel moduleName rootName)

        toDef (name, accessExpr) =
            Opt.Def (Opt.Facts home) name accessExpr
    in
        map toDef (patternToSubstitutions rootExpr pattern)



-- TURN A PATTERN INTO A BUNCH OF SUBSTITUTIONS


patternToSubstitutions :: Opt.Expr -> P.Canonical -> [(String, Opt.Expr)]
patternToSubstitutions expr (A.A _ pattern) =
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
        (alias, expr) : patternToSubstitutions expr realPattern

    P.Data _ patterns ->
        concat (zipWith (patternToSubstitutions . Opt.CtorAccess expr) [0..] patterns)



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
    Can.Literal lit ->
        pure (Opt.Literal lit)

    Can.Var name ->
        if name == Var.inCore ["Debug"] "crash" then
            do  home <- Env.getHome
                pure (Opt.Crash home region Nothing)

        else
            pure (Opt.Var name)

    Can.List elements ->
        Opt.List
          <$> T.traverse justConvert elements

    Can.Binop op leftExpr rightExpr ->
        optimizeBinop context region op leftExpr rightExpr

    Can.Lambda _ _ ->
        let
            (patterns, body) =
                Can.collectLambdas annExpr
        in
            uncurry Opt.Function <$> optimizeFunction Nothing patterns body

    Can.App _ _ ->
        let
            (func:args) =
                Can.collectApps annExpr
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

    Can.If branches finally ->
        let
            crawlBranch (cond,branch) =
                (,) <$> justConvert cond <*> keepLooking branch
        in
            Opt.If
              <$> T.traverse crawlBranch branches
              <*> keepLooking finally

    Can.Let defs body ->
        do  optDefs <- concat <$> T.traverse (optimizeDef False) defs
            Opt.Let optDefs <$> keepLooking body

    Can.Case expr branches ->
        do  optExpr <- optimizeExpr Nothing expr
            variantDict <- Env.getVariantDict

            name <- Env.freshName
            optBranches <- T.traverse (optimizeBranch context region name) branches
            let optCase = Case.optimize variantDict name optBranches
            return $ Opt.Let [ Opt.Def Opt.dummyFacts name optExpr ] optCase

    Can.Ctor name exprs ->
        Opt.Ctor name <$> T.traverse justConvert exprs

    Can.Access record field ->
        Opt.Access
          <$> justConvert record
          <*> pure field

    Can.Update record fields ->
        Opt.Update
          <$> justConvert record
          <*> T.traverse (mapSnd justConvert) fields

    Can.Record fields ->
        Opt.Record
          <$> T.traverse (mapSnd justConvert) fields

    Can.Cmd moduleName ->
        pure (Opt.Cmd moduleName)

    Can.Sub moduleName ->
        pure (Opt.Sub moduleName)

    Can.OutgoingPort name tipe ->
        pure (Opt.OutgoingPort name tipe)

    Can.IncomingPort name tipe ->
        pure (Opt.IncomingPort name tipe)

    Can.Program kind expr ->
        Opt.Program kind <$> justConvert expr

    Can.SaveEnv _ _ ->
        error "save_the_environment should never make it to optimization phase"

    Can.GLShader uid src gltipe ->
        pure (Opt.GLShader uid src gltipe)


mapSnd :: M.Functor box => (a -> box b) -> (x, a) -> box (x, b)
mapSnd func (x, a) =
  (,) x <$> func a



-- OPTIMIZE CASE BRANCHES


optimizeBranch
    :: Context
    -> R.Region
    -> String
    -> (P.Canonical, Can.Expr)
    -> Env.Optimizer (P.Canonical, Opt.Expr)
optimizeBranch context region exprName (rawPattern, canBranch) =
  let
    root =
      Opt.Var (Var.Canonical Var.Local exprName)
  in
    do  optBranch <- optimizeExpr context canBranch
        (pattern, branch) <- tagCrashBranch region rawPattern optBranch

        let substitutions =
              Map.fromList (patternToSubstitutions root pattern)

        (,) pattern <$> Inline.inline substitutions branch


tagCrashBranch
    :: R.Region
    -> P.Canonical
    -> Opt.Expr
    -> Env.Optimizer (P.Canonical, Opt.Expr)
tagCrashBranch region pattern@(A.A pr _) expr =
  case expr of
    Opt.Call (Opt.Crash home _ _) [arg] ->
        do  name <- Env.freshName
            return
              ( A.A pr (P.Alias name pattern)
              , Opt.Call (Opt.Crash home region (Just (Opt.Var (Var.local name)))) [arg]
              )

    _ ->
        return (pattern, expr)



-- DETECT TAIL CALL


isTailCall :: Context -> Can.Expr -> Int -> Maybe (String, [String])
isTailCall context (A.A _ function) arity =
    case (context, function) of
      ( Just (ctxName, ctxArity, argNames), Can.Var (Var.Canonical home name) ) ->

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


depattern :: P.Canonical -> Can.Expr -> Env.Optimizer (String, Can.Expr)
depattern canPattern@(A.A _ pattern) expr =
  let
    ann =
      A.A (error "the annotation added in 'depattern' should not be observed!")

    caseExpr e branches =
      ann (Can.Case e branches)

    var name =
      ann (Can.Var (Var.local name))

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
    binop = ann (Can.Binop op leftExpr rightExpr)
  in
  if op == forwardApply then

      optimizeExpr context (collect id left forwardApply binop)

  else if op == backwardApply then

      optimizeExpr context (collect id right backwardApply binop)

  else if op == forwardCompose then

      do  var <- Env.freshName
          let makeRoot func = ann (Can.App func (ann (Can.Var (Var.local var))))
          let body = collect makeRoot left forwardCompose binop
          optimizeExpr context (ann (Can.Lambda (ann (P.Var var)) body))

  else if op == backwardCompose then

      do  var <- Env.freshName
          let makeRoot func = ann (Can.App func (ann (Can.Var (Var.local var))))
          let body = collect makeRoot right backwardCompose binop
          optimizeExpr context (ann (Can.Lambda (ann (P.Var var)) body))

  else if op == Var.Canonical Var.BuiltIn "::" then

      optimizeExpr context (ann (Can.Ctor "::" [ leftExpr, rightExpr ]))

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
    Can.Binop op leftExpr rightExpr | op == desiredOp ->
        let
          (func, arg) = assoc leftExpr rightExpr
        in
          A.A ann (Can.App func (collect makeRoot assoc desiredOp arg))

    _ ->
        makeRoot annExpr
