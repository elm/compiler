{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize (optimize) where

import Control.Arrow (second)
import qualified Control.Monad as M
import Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Expression.Canonical as Can
import qualified AST.Expression.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Optimize.Case as Case
import qualified Optimize.DecisionTree as DT
import qualified Optimize.Environment as Env
import qualified Optimize.Inline as Inline
import qualified Optimize.Port as Port
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- OPTIMIZE


optimize :: DT.VariantDict -> ModuleName.Canonical -> [Can.Def] -> [(Text, Opt.Decl)]
optimize variantDict home defs =
  concat $ zipWith (optimizeDecl variantDict home) [1 .. length defs] defs



-- OPTIMIZE DECLS


optimizeDecl :: DT.VariantDict -> ModuleName.Canonical -> Int -> Can.Def -> [(Text, Opt.Decl)]
optimizeDecl variantDict home index (Can.Def _ pattern@(A.A _ ptrn) expression _) =
  let
    (rawArgs, rawBody) =
      Can.collectLambdas expression
  in
    case (ptrn, rawArgs) of
      (P.Var name, _) ->
        (:[]) $ Env.run variantDict home $
          optimizeNamedDef name rawArgs rawBody

      (_, []) ->
        let
          name = "_" <> Text.pack (show index)
          var = Var.Global home name
          deps = Set.singleton var
          decl =
            Env.run variantDict home $
              ((,) name . Opt.Def) <$> optimizeExpr Nothing rawBody
          destructors =
            destruct (Opt.VarGlobal var) pattern
        in
          decl : map (second (Opt.Decl deps Set.empty Nothing)) destructors

      _ ->
        error "bug manifesting in Optimize.optimizeDecl, please report"



-- OPTIMIZE DEFS


optimizeLetDef :: Can.Def -> Env.Optimizer [(Text, Opt.Def)]
optimizeLetDef (Can.Def _ pattern@(A.A _ ptrn) expression _) =
  let
    (rawArgs, rawBody) =
      Can.collectLambdas expression
  in
    case (ptrn, rawArgs) of
      (P.Var name, _) ->
        do  def <- optimizeNamedDef name rawArgs rawBody
            return [def]

      (_, []) ->
        do  name <- Env.freshName
            body <- optimizeExpr Nothing rawBody
            return $ (name, Opt.Def body) : destruct (Opt.VarLocal name) pattern

      _ ->
        error "bug manifesting in Optimize.optimizeLetDef, please report"


optimizeNamedDef :: Text -> [P.Canonical] -> Can.Expr -> Env.Optimizer (Text, Opt.Def)
optimizeNamedDef name rawArgs rawBody =
  case rawArgs of
    [] ->
      do  hasTailCall <- Env.getTailCall
          body <- optimizeExpr Nothing rawBody
          Env.setTailCall hasTailCall
          return (name, Opt.Def body)

    _ : _ ->
      do  htc <- Env.getTailCall
          Env.setTailCall False

          (args, body) <- optimizeFunction (Just name) rawArgs rawBody

          isTailCall <- Env.getTailCall
          Env.setTailCall htc

          return $ (,) name $
            if isTailCall then
              Opt.TailDef args body
            else
              Opt.Def (Opt.Function args body)


destruct :: Opt.Expr -> P.Canonical -> [(Text, Opt.Def)]
destruct root pattern =
  map (second Opt.Def) $
    patternToSubstitutions root pattern



-- OPTIMIZE FUNCTION


optimizeFunction
    :: Maybe Text
    -> [P.Canonical]
    -> Can.Expr
    -> Env.Optimizer ([Text], Opt.Expr)
optimizeFunction maybeName patterns givenBody =
  Env.indirectly $
  do  (argNames, body) <- M.foldM depatternArgs ([], givenBody) (reverse patterns)
      optBody <- optimizeExpr (makeContext argNames =<< maybeName) body
      return (argNames, optBody)


makeContext :: [Text] -> Text -> Context
makeContext argNames name =
  Just (name, length argNames, argNames)


depatternArgs :: ([Text], Can.Expr) -> P.Canonical -> Env.Optimizer ([Text], Can.Expr)
depatternArgs (names, rawExpr) ptrn =
  do  (name, expr) <- depattern ptrn rawExpr
      return (name : names, expr)



-- TURN A PATTERN INTO A BUNCH OF SUBSTITUTIONS


patternToSubstitutions :: Opt.Expr -> P.Canonical -> [(Text, Opt.Expr)]
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

    P.Ctor _ patterns ->
        concat (zipWith (patternToSubstitutions . Opt.CtorAccess expr) [0..] patterns)



-- OPTIMIZE EXPRESSIONS


type Context = Maybe (Text, Int, [Text])


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

    Can.Var var ->
        optimizeVariable region var

    Can.List elements ->
        Opt.List
          <$> traverse justConvert elements

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
            case getTailCallInfo context func (length args) of
              Just (name, argNames) ->
                  do  optArgs <- traverse justConvert args
                      Env.setTailCall True
                      return (Opt.TailCall name argNames optArgs)

              Nothing ->
                  do  hasTailCall <- Env.getTailCall
                      optFunc <- justConvert func
                      optArgs <- traverse justConvert args
                      Env.setTailCall hasTailCall
                      return (Opt.Call optFunc optArgs)

    Can.If branches finally ->
        let
            crawlBranch (cond,branch) =
                (,) <$> justConvert cond <*> keepLooking branch
        in
            Opt.If
              <$> traverse crawlBranch branches
              <*> keepLooking finally

    Can.Let defs body ->
        do  optDefs <- concat <$> traverse optimizeLetDef defs
            Opt.Let optDefs <$> keepLooking body

    Can.Case expr branches ->
        do  optExpr <- optimizeExpr Nothing expr
            variantDict <- Env.getVariantDict

            name <- Env.freshName
            optBranches <- traverse (optimizeBranch context region name) branches
            let optCase = Case.optimize variantDict name optBranches
            return $ Opt.Let [ (name, Opt.Def optExpr) ] optCase

    Can.Ctor (Var.Canonical _ name) exprs ->
        Opt.Ctor name <$> traverse justConvert exprs

    Can.Access record field ->
        Opt.Access
          <$> justConvert record
          <*> pure field

    Can.Update record fields ->
        Opt.Update
          <$> justConvert record
          <*> traverse (traverse justConvert) fields

    Can.Record fields ->
        Opt.Record
          <$> traverse (traverse justConvert) fields

    Can.Cmd home manager ->
        Opt.Cmd home <$> Env.registerEffects home manager

    Can.Sub home manager ->
        Opt.Sub home <$> Env.registerEffects home manager

    Can.OutgoingPort name tipe ->
        Opt.OutgoingPort name <$> Port.toEncoder tipe

    Can.IncomingPort name tipe ->
        Opt.IncomingPort name <$> Port.toDecoder tipe

    Can.Program main expr ->
        Opt.Program <$> optimizeMain main <*> justConvert expr

    Can.SaveEnv _ _ ->
        error "save_the_environment should never make it to optimization phase"

    Can.GLShader _ src _ ->
        pure (Opt.GLShader src)



-- OPTIMIZE VARIABLE


optimizeVariable :: R.Region -> Var.Canonical -> Env.Optimizer Opt.Expr
optimizeVariable region (Var.Canonical home name) =
  case home of
    Var.BuiltIn ->
      pure (Opt.VarLocal name)

    Var.Local ->
      pure (Opt.VarLocal name)

    Var.TopLevel modul ->
      do  let var = Var.Global modul name
          Env.register var
          pure (Opt.VarGlobal var)

    Var.Module modul ->
      do  let var = Var.Global modul name
          Env.register var
          if name == "crash" && modul == ModuleName.inCore "Debug"
            then
              do  here <- Env.getHome
                  pure (Opt.Crash here region Nothing)
            else
              pure (Opt.VarGlobal var)



-- OPTIMIZE MAIN


optimizeMain :: Can.Main -> Env.Optimizer Opt.Main
optimizeMain main =
  case main of
    Can.Static ->
      return Opt.Static

    Can.Dynamic tipe ->
      Opt.Dynamic <$> Port.toFlagsDecoder tipe



-- OPTIMIZE CASE BRANCHES


optimizeBranch
    :: Context
    -> R.Region
    -> Text
    -> (P.Canonical, Can.Expr)
    -> Env.Optimizer (P.Canonical, Opt.Expr)
optimizeBranch context region exprName (rawPattern, canBranch) =
  do  optBranch <- optimizeExpr context canBranch
      (pattern, branch) <- tagCrashBranch region rawPattern optBranch

      let substitutions = Map.fromList $
            patternToSubstitutions (Opt.VarLocal exprName) pattern

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
              , Opt.Call (Opt.Crash home region (Just (Opt.VarLocal name))) [arg]
              )

    _ ->
        return (pattern, expr)



-- DETECT TAIL CALL


getTailCallInfo :: Context -> Can.Expr -> Int -> Maybe (Text, [Text])
getTailCallInfo context (A.A _ function) arity =
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


depattern :: P.Canonical -> Can.Expr -> Env.Optimizer (Text, Can.Expr)
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

    P.Ctor _ _ ->
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

  else if op == cons then

      optimizeExpr context (ann (Can.Ctor cons [ leftExpr, rightExpr ]))

  else

      do  optLeft <- optimizeExpr Nothing leftExpr
          optRight <- optimizeExpr Nothing rightExpr
          makeOptBinop op optLeft optRight


-- left-associative ((x |> f) |> g)
forwardApply :: Var.Canonical
forwardApply =
  Var.inCore "Basics" "|>"


-- right-associative (g <| (f <| x))
backwardApply :: Var.Canonical
backwardApply =
  Var.inCore "Basics" "<|"


-- left-associative ((f >> g) >> h)
forwardCompose :: Var.Canonical
forwardCompose =
  Var.inCore "Basics" ">>"


-- right-associative (h << (g << f))
backwardCompose :: Var.Canonical
backwardCompose =
  Var.inCore "Basics" "<<"


cons :: Var.Canonical
cons =
  Var.Canonical Var.BuiltIn "::"


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


makeOptBinop :: Var.Canonical -> Opt.Expr -> Opt.Expr -> Env.Optimizer Opt.Expr
makeOptBinop (Var.Canonical home op) leftExpr rightExpr =
  case home of
    Var.Local ->
      error "bug manifesting in Optimize.makeOptBinop"

    Var.BuiltIn ->
      error "bug manifesting in Optimize.makeOptBinop"

    Var.Module name ->
      do  let var = Var.Global name op
          Env.register var
          return $ Opt.Binop var leftExpr rightExpr

    Var.TopLevel name ->
      do  let var = Var.Global name op
          Env.register var
          return $ Opt.Binop var leftExpr rightExpr
