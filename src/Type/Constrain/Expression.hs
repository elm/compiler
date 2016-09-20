{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Expression where

import Control.Arrow (second)
import qualified Control.Monad as Monad
import qualified Data.Map as Map

import qualified AST.Expression.General as E
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Literal as Lit
import qualified AST.Pattern as P
import qualified AST.Type as ST
import qualified AST.Variable as V
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import qualified Type.Constrain.Effects as Effects
import qualified Type.Constrain.Literal as Literal
import qualified Type.Constrain.Pattern as Pattern
import qualified Type.Environment as Env
import Type.Type hiding (Descriptor(..))



constrain :: Env.Env -> Canonical.Expr -> Type -> IO TypeConstraint
constrain env annotatedExpr@(A.A region expression) tipe =
  case expression of
    E.Literal lit ->
      Literal.constrain env region lit tipe

    E.Cmd _ ->
      return CTrue

    E.Sub _ ->
      return CTrue

    E.OutgoingPort _ _ ->
      return CTrue

    E.IncomingPort _ _ ->
      return CTrue

    E.Program _ _ ->
      error "DANGER - Program AST nodes should not be in type inference."

    E.SaveEnv moduleName effects ->
      Effects.constrain env moduleName effects

    E.GLShader _uid _src gltipe ->
      exists $ \attr ->
      exists $ \unif ->
        let
          shaderTipe a u v =
            Env.getType env "WebGL.Shader" <| a <| u <| v

          glToType glTipe =
            Env.getType env (Lit.glTipeName glTipe)

          makeRec accessor baseRec =
            let decls = accessor gltipe
            in
              if Map.size decls == 0 then
                  baseRec
              else
                  record (Map.map glToType decls) baseRec

          attribute = makeRec Lit.attribute attr
          uniform = makeRec Lit.uniform unif
          varying = makeRec Lit.varying (TermN EmptyRecord1)
        in
          return (CEqual Error.Shader region (shaderTipe attribute uniform varying) tipe)

    E.Var var ->
      return (CInstance region (V.toString var) tipe)

    E.ExplicitList exprs ->
      constrainList env region exprs tipe

    E.Binop op leftExpr rightExpr ->
      constrainBinop env region op leftExpr rightExpr tipe

    E.Lambda pattern body ->
      exists $ \argType ->
      exists $ \resType ->
        do  (Pattern.Info headers vars cons) <- Pattern.constrain env pattern argType
            bodyCon <- constrain env body resType
            return $
              ex vars (CLet [monoscheme headers] (cons /\ bodyCon))
              /\ CEqual Error.Lambda region (argType ==> resType) tipe

    E.App _ _ ->
      let
        (f:args) = E.collectApps annotatedExpr
      in
        constrainApp env region f args tipe

    E.If branches finally ->
      constrainIf env region branches finally tipe

    E.Case expr branches ->
      constrainCase env region expr branches tipe

    E.Data name exprs ->
      do  vars <- Monad.forM exprs $ \_ -> mkVar Nothing
          let pairs = zip exprs (map VarN vars)
          (ctipe, cs) <- Monad.foldM step (tipe, CTrue) (reverse pairs)
          return $ ex vars (cs /\ CInstance region name ctipe)
      where
        step (t,c) (e,x) =
            do  c' <- constrain env e x
                return (x ==> t, c /\ c')

    E.Access expr field ->
      exists $ \recordType ->
      exists $ \ext ->
        do  recordCon <- constrain env expr recordType
            let maybeBody = E.collectFields expr
            let desiredType = record (Map.singleton field tipe) ext
            let fieldCon = CEqual (Error.Access maybeBody field) region recordType desiredType
            return $ recordCon /\ fieldCon

    E.Update expr fields ->
      exists $ \t ->
        do  oldVars <- mapM (\_ -> mkVar Nothing) fields
            let oldFields = Map.fromList (zip (map fst fields) (map VarN oldVars))
            cOld <- ex oldVars <$> constrain env expr (record oldFields t)

            newVars <- mapM (\_ -> mkVar Nothing) fields
            let newFields = Map.fromList (zip (map fst fields) (map VarN newVars))
            let cNew = CEqual Error.Record region (record newFields t) tipe

            cs <- Monad.zipWithM (constrain env) (map snd fields) (map VarN newVars)

            return $ cOld /\ ex newVars (CAnd (cNew : cs))

    E.Record fields ->
      do  vars <- Monad.forM fields (\_ -> mkVar Nothing)
          fieldCons <-
              Monad.zipWithM
                  (constrain env)
                  (map snd fields)
                  (map VarN vars)
          let fields' = Map.fromList (zip (map fst fields) (map VarN vars))
          let recordType = record fields' (TermN EmptyRecord1)
          return (ex vars (CAnd (fieldCons ++ [CEqual Error.Record region recordType tipe])))

    E.Let defs body ->
      do  bodyCon <- constrain env body tipe

          (Info schemes rqs fqs headers c2 c1) <-
              Monad.foldM
                  (constrainDef env)
                  (Info [] [] [] Map.empty CTrue CTrue)
                  (concatMap expandPattern defs)

          let letScheme =
                [ Scheme rqs fqs (CLet [monoscheme headers] c2) headers ]

          return $ CLet schemes (CLet letScheme (c1 /\ bodyCon))



-- CONSTRAIN APP


constrainApp
    :: Env.Env
    -> R.Region
    -> Canonical.Expr
    -> [Canonical.Expr]
    -> Type
    -> IO TypeConstraint
constrainApp env region f args tipe =
  do  funcVar <- mkVar Nothing
      funcCon <- constrain env f (VarN funcVar)

      (vars, argCons, numberOfArgsCons, argMatchCons, _, returnVar) <-
          argConstraints env maybeName region (length args) funcVar 1 args

      let returnCon =
            CEqual (Error.Function maybeName) region (VarN returnVar) tipe

      return $ ex (funcVar : vars) $
        CAnd (funcCon : argCons ++ numberOfArgsCons ++ argMatchCons ++ [returnCon])
  where
    maybeName =
      case f of
        A.A _ (E.Var canonicalName) ->
            Just canonicalName

        _ ->
          Nothing


argConstraints
    :: Env.Env
    -> Maybe V.Canonical
    -> R.Region
    -> Int
    -> Variable
    -> Int
    -> [Canonical.Expr]
    -> IO ([Variable], [TypeConstraint], [TypeConstraint], [TypeConstraint], Maybe R.Region, Variable)
argConstraints env name region totalArgs overallVar index args =
  case args of
    [] ->
      return ([], [], [], [], Nothing, overallVar)

    expr@(A.A subregion _) : rest ->
      do  argVar <- mkVar Nothing
          argCon <- constrain env expr (VarN argVar)
          argIndexVar <- mkVar Nothing
          localReturnVar <- mkVar Nothing

          (vars, argConRest, numberOfArgsRest, argMatchRest, restRegion, returnVar) <-
              argConstraints env name region totalArgs localReturnVar (index + 1) rest

          let arityRegion =
                maybe subregion (R.merge subregion) restRegion

          let numberOfArgsCon =
                CEqual
                  (Error.FunctionArity name (index - 1) totalArgs arityRegion)
                  region
                  (VarN argIndexVar ==> VarN localReturnVar)
                  (VarN overallVar)

          let argMatchCon =
                CEqual
                  (Error.UnexpectedArg name index totalArgs subregion)
                  region
                  (VarN argIndexVar)
                  (VarN argVar)

          return
            ( argVar : argIndexVar : localReturnVar : vars
            , argCon : argConRest
            , numberOfArgsCon : numberOfArgsRest
            , argMatchCon : argMatchRest
            , Just arityRegion
            , returnVar
            )



-- CONSTRAIN BINOP


constrainBinop
    :: Env.Env
    -> R.Region
    -> V.Canonical
    -> Canonical.Expr
    -> Canonical.Expr
    -> Type
    -> IO TypeConstraint
constrainBinop env region op leftExpr@(A.A leftRegion _) rightExpr@(A.A rightRegion _) tipe =
  do  leftVar <- mkVar Nothing
      rightVar <- mkVar Nothing

      leftCon <- constrain env leftExpr (VarN leftVar)
      rightCon <- constrain env rightExpr (VarN rightVar)

      leftVar' <- mkVar Nothing
      rightVar' <- mkVar Nothing
      answerVar <- mkVar Nothing

      let opType = VarN leftVar' ==> VarN rightVar' ==> VarN answerVar

      return $
        ex [leftVar,rightVar,leftVar',rightVar',answerVar] $ CAnd $
          [ leftCon
          , rightCon
          , CInstance region (V.toString op) opType
          , CEqual (Error.BinopLeft op leftRegion) region (VarN leftVar') (VarN leftVar)
          , CEqual (Error.BinopRight op rightRegion) region (VarN rightVar') (VarN rightVar)
          , CEqual (Error.Binop op) region (VarN answerVar) tipe
          ]



-- CONSTRAIN LISTS


constrainList :: Env.Env -> R.Region -> [Canonical.Expr] -> Type -> IO TypeConstraint
constrainList env region exprs tipe =
  do  (exprInfo, exprCons) <-
          unzip <$> mapM elementConstraint exprs

      (vars, cons) <- pairCons region Error.ListElement varToCon exprInfo

      return $ ex vars (CAnd (exprCons ++ cons))
  where
    elementConstraint expr@(A.A region' _) =
      do  var <- mkVar Nothing
          con <- constrain env expr (VarN var)
          return ( (var, region'), con )

    varToCon var =
      CEqual Error.List region (Env.getType env "List" <| VarN var) tipe



-- CONSTRAIN IF EXPRESSIONS


constrainIf
    :: Env.Env
    -> R.Region
    -> [(Canonical.Expr, Canonical.Expr)]
    -> Canonical.Expr
    -> Type
    -> IO TypeConstraint
constrainIf env region branches finally tipe =
  do  let (conditions, branchExprs) =
            second (++ [finally]) (unzip branches)

      (condVars, condCons) <-
          unzip <$> mapM constrainCondition conditions

      (branchInfo, branchExprCons) <-
          unzip <$> mapM constrainBranch branchExprs

      (vars,cons) <- branchCons branchInfo

      return $ ex (condVars ++ vars) (CAnd (condCons ++ branchExprCons ++ cons))
  where
    bool =
      Env.getType env "Bool"

    constrainCondition condition@(A.A condRegion _) =
      do  condVar <- mkVar Nothing
          condCon <- constrain env condition (VarN condVar)
          let boolCon = CEqual Error.IfCondition condRegion (VarN condVar) bool
          return (condVar, CAnd [ condCon, boolCon ])

    constrainBranch expr@(A.A branchRegion _) =
      do  branchVar <- mkVar Nothing
          exprCon <- constrain env expr (VarN branchVar)
          return
            ( (branchVar, branchRegion)
            , exprCon
            )

    branchCons branchInfo =
      case branchInfo of
        [(thenVar, _), (elseVar, _)] ->
            return
              ( [thenVar,elseVar]
              , [ CEqual Error.IfBranches region (VarN thenVar) (VarN elseVar)
                , varToCon thenVar
                ]
              )

        _ ->
            pairCons region Error.MultiIfBranch varToCon branchInfo

    varToCon var =
      CEqual Error.If region (VarN var) tipe



-- CONSTRAIN CASE EXPRESSIONS


constrainCase
    :: Env.Env
    -> R.Region
    -> Canonical.Expr
    -> [(P.Canonical, Canonical.Expr)]
    -> Type
    -> IO TypeConstraint
constrainCase env region expr branches tipe =
  do  exprVar <- mkVar Nothing
      exprCon <- constrain env expr (VarN exprVar)

      (branchInfo, branchExprCons) <-
          unzip <$> mapM (branch (VarN exprVar)) branches

      (vars, cons) <- pairCons region Error.CaseBranch varToCon branchInfo

      return $ ex (exprVar : vars) (CAnd (exprCon : branchExprCons ++ cons))
  where
    branch patternType (pattern, branchExpr@(A.A branchRegion _)) =
        do  branchVar <- mkVar Nothing
            scheme <- Pattern.infoToScheme <$> Pattern.constrain env pattern patternType
            branchCon <- constrain env branchExpr (VarN branchVar)
            return
                ( (branchVar, branchRegion)
                , CLet [scheme] branchCon
                )

    varToCon var =
      CEqual Error.Case region tipe (VarN var)



-- COLLECT PAIRS


data Pair = Pair
    { _index :: Int
    , _var1 :: Variable
    , _var2 :: Variable
    , _region :: R.Region
    }


pairCons
    :: R.Region
    -> (Int -> R.Region -> Error.Hint)
    -> (Variable -> TypeConstraint)
    -> [(Variable, R.Region)]
    -> IO ([Variable], [TypeConstraint])
pairCons region pairHint varToCon items =
  let
    pairToCon (Pair index var1 var2 subregion) =
      CEqual (pairHint index subregion) region (VarN var1) (VarN var2)
  in
  case collectPairs 2 items of
    Nothing ->
      do  var <- mkVar Nothing
          return ([var], [varToCon var])

    Just (pairs, var) ->
      return (map fst items, map pairToCon pairs ++ [varToCon var])


collectPairs :: Int -> [(Variable, R.Region)] -> Maybe ([Pair], Variable)
collectPairs index items =
  case items of
    [] ->
        Nothing

    (var,_) : [] ->
        Just ([], var)

    (var,_) : rest@((var',region) : _) ->
        do  (pairs, summaryVar) <- collectPairs (index+1) rest
            return (Pair index var var' region : pairs, summaryVar)



-- EXPAND PATTERNS


expandPattern :: Canonical.Def -> [Canonical.Def]
expandPattern def@(Canonical.Def facts lpattern expr maybeType) =
  let
    (A.A patternRegion pattern) =
      lpattern
  in
  case pattern of
    P.Var _ ->
        [def]

    _ ->
        mainDef : map toDef vars
      where
        vars =
          P.boundVarList lpattern

        combinedName =
          "$" ++ concat vars

        pvar name =
          A.A patternRegion (P.Var name)

        localVar name =
          A.A patternRegion (E.localVar name)

        mainDef =
          Canonical.Def facts (pvar combinedName) expr maybeType

        toDef name =
          let
            extract =
              E.Case (localVar combinedName) [(lpattern, localVar name)]
          in
            Canonical.Def facts (pvar name) (A.A patternRegion extract) Nothing



-- CONSTRAIN DEFINITIONS


data Info = Info
    { iSchemes :: [TypeScheme]
    , iRigid :: [Variable]
    , iFlex :: [Variable]
    , iHeaders :: Map.Map String (A.Located Type)
    , iC2 :: TypeConstraint
    , iC1 :: TypeConstraint
    }


constrainDef :: Env.Env -> Info -> Canonical.Def -> IO Info
constrainDef env info (Canonical.Def defRegion (A.A patternRegion pattern) expr maybeTipe) =
  case (pattern, maybeTipe) of
    (P.Var name, Nothing) ->
        constrainUnannotatedDef env patternRegion name expr info

    (P.Var name, Just (A.A _ tipe)) ->
        constrainAnnotatedDef env defRegion patternRegion name expr tipe info

    _ ->
        error "canonical definitions must not have complex patterns as names in the contstraint generation phase"


constrainUnannotatedDef
    :: Env.Env
    -> R.Region
    -> String
    -> Canonical.Expr
    -> Info
    -> IO Info
constrainUnannotatedDef env patternRegion name expr info =
  do  v <- mkVar Nothing
      let tipe = VarN v
      defCon <- constrain env expr tipe
      return $ info
          { iFlex = v : iFlex info
          , iHeaders = Map.insert name (A.A patternRegion tipe) (iHeaders info)
          , iC2 = defCon /\ iC2 info
          }


constrainAnnotatedDef
    :: Env.Env
    -> R.Region
    -> R.Region
    -> String
    -> Canonical.Expr
    -> ST.Canonical
    -> Info
    -> IO Info
constrainAnnotatedDef env defRegion region name expr tipe info =
  do  (flexType, flexVars) <- Env.instantiateType Flex env tipe

      let scheme =
            Scheme
              { _rigidQuantifiers = []
              , _flexibleQuantifiers = Map.elems flexVars
              , _constraint = CTrue
              , _header = Map.singleton name (A.A region flexType)
              }

      defCon <- constrainAnnDefHelp expr tipe (ArgInfo name defRegion env [] [])

      return $ info
          { iSchemes = scheme : iSchemes info
          , iC1 = iC1 info /\ defCon
          }


data ArgInfo =
  ArgInfo
    { _name :: String
    , _def :: R.Region
    , _env :: Env.Env
    , _args :: [(P.Canonical, Type)]
    , _vars :: [Variable]
    }


constrainAnnDefHelp :: Canonical.Expr -> ST.Canonical -> ArgInfo -> IO TypeConstraint
constrainAnnDefHelp expr tipe (ArgInfo name defRegion env args vars) =
  case (expr, tipe) of
    (A.A _ (E.Lambda arg result), ST.Lambda argType resultType) ->
      do  (rigidArgType, newVars) <- Env.instantiateType Rigid env argType

          constrainAnnDefHelp result resultType $
            ArgInfo
              { _name = name
              , _def = defRegion
              , _env = Env.addValues newVars env
              , _args = (arg, rigidArgType) : args
              , _vars = Map.elems newVars ++ vars
              }

    (A.A region _, _) ->
      do  (rigidType, newVars) <- Env.instantiateType Rigid env tipe

          let finalEnv = Env.addValues newVars env
          let finalArgs = reverse args
          let rigidVars = Map.elems newVars ++ vars

          (Pattern.Info headers argVars argCons) <-
            Pattern.joinInfos <$> mapM (uncurry (Pattern.constrain finalEnv)) finalArgs

          resultVar <- mkVar Nothing
          let resultType = VarN resultVar
          defCon <- constrain finalEnv expr resultType

          let sharedArity = length args
          let typeArity = sharedArity + length (ST.collectLambdas tipe) - 1
          let argsArity = sharedArity + length (fst (E.collectLambdas expr))
          let hint = Error.ReturnType name typeArity argsArity region
          let resultCon =
                CEqual hint defRegion rigidType resultType

          return $ forall rigidVars $ ex (resultVar : argVars) $ CLet [monoscheme headers] $
            CAnd [ argCons, defCon, resultCon ]
