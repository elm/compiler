{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Expression where

import Control.Applicative ((<$>))
import qualified Control.Monad as Monad
import qualified Data.List as List
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
import qualified Type.Constrain.Literal as Literal
import qualified Type.Constrain.Pattern as Pattern
import qualified Type.Environment as Env
import qualified Type.Fragment as Fragment
import Type.Type hiding (Descriptor(..))


constrain
    :: Env.Environment
    -> Canonical.Expr
    -> Type
    -> IO TypeConstraint
constrain env (A.A region expression) tipe =
    let list t = Env.get env Env.types "List" <| t
        (===) = CEqual Error.None region
        (<?) = CInstance region
    in
    case expression of
      E.Literal lit ->
          Literal.constrain env region lit tipe

      E.GLShader _uid _src gltipe ->
          exists $ \attr ->
          exists $ \unif ->
            let
                shaderTipe a u v =
                    Env.get env Env.types "WebGL.Shader" <| a <| u <| v

                glTipe =
                    Env.get env Env.types . Lit.glTipeName

                makeRec accessor baseRec =
                    let decls = accessor gltipe
                    in
                      if Map.size decls == 0
                        then baseRec
                        else record (Map.map (\t -> [glTipe t]) decls) baseRec

                attribute = makeRec Lit.attribute attr
                uniform = makeRec Lit.uniform unif
                varying = makeRec Lit.varying (termN EmptyRecord1)
            in
                return (tipe === shaderTipe attribute uniform varying)

      E.Var var ->
          let name = V.toString var
          in
              return (if name == E.saveEnvName then CSaveEnv else name <? tipe)

      E.Range lowExpr highExpr ->
          existsNumber $ \n ->
              do  lowCon <- constrain env lowExpr n
                  highCon <- constrain env highExpr n
                  return $ CAnd [lowCon, highCon, list n === tipe]

      E.ExplicitList exprs ->
          constrainList env region exprs tipe

      E.Binop op leftExpr rightExpr ->
          constrainBinop env region op leftExpr rightExpr tipe

      E.Lambda pattern body ->
          exists $ \argType ->
          exists $ \resType ->
              do  fragment <- Pattern.constrain env pattern argType
                  bodyCon <- constrain env body resType
                  let con =
                        ex (Fragment.vars fragment)
                            (CLet [monoscheme (Fragment.typeEnv fragment)]
                                  (Fragment.typeConstraint fragment /\ bodyCon)
                            )
                  return $ con /\ tipe === (argType ==> resType)

      E.App func@(A.A funcRegion _) arg@(A.A argRegion _) ->
          do  argVar <- variable Flexible
              argCon <- constrain env arg (varN argVar)

              funcVar <- variable Flexible
              funcCon <- constrain env func (varN funcVar)

              argVar' <- variable Flexible
              resultVar <- variable Flexible

              return $ ex [funcVar, argVar, argVar', resultVar] $ CAnd $
                [ argCon
                , funcCon
                , CEqual (Error.ExtraArgument funcRegion) region
                    (varN funcVar)
                    (varN argVar' ==> varN resultVar)
                , CEqual (Error.BadArgument argRegion) region (varN argVar') (varN argVar)
                , tipe === varN resultVar
                ]

      E.MultiIf branches ->
          constrainIf env region branches tipe

      E.Case expr branches ->
          constrainCase env region expr branches tipe

      E.Data name exprs ->
          do  vars <- Monad.forM exprs $ \_ -> variable Flexible
              let pairs = zip exprs (map varN vars)
              (ctipe, cs) <- Monad.foldM step (tipe, CTrue) (reverse pairs)
              return $ ex vars (cs /\ name <? ctipe)
          where
            step (t,c) (e,x) =
                do  c' <- constrain env e x
                    return (x ==> t, c /\ c')

      E.Access expr label ->
          exists $ \t ->
              constrain env expr (record (Map.singleton label [tipe]) t)

      E.Remove expr label ->
          exists $ \t ->
              constrain env expr (record (Map.singleton label [t]) tipe)

      E.Insert expr label value ->
          exists $ \valueType ->
          exists $ \recordType ->
              do  valueCon <- constrain env value valueType
                  recordCon <- constrain env expr recordType
                  let newRecordType =
                        record (Map.singleton label [valueType]) recordType
                  return (CAnd [valueCon, recordCon, tipe === newRecordType])

      E.Modify expr fields ->
          exists $ \t ->
              do  oldVars <- Monad.forM fields $ \_ -> variable Flexible
                  let oldFields = ST.fieldMap (zip (map fst fields) (map varN oldVars))
                  cOld <- ex oldVars <$> constrain env expr (record oldFields t)

                  newVars <- Monad.forM fields $ \_ -> variable Flexible
                  let newFields = ST.fieldMap (zip (map fst fields) (map varN newVars))
                  let cNew = tipe === record newFields t

                  cs <- Monad.zipWithM (constrain env) (map snd fields) (map varN newVars)

                  return $ cOld /\ ex newVars (CAnd (cNew : cs))

      E.Record fields ->
          do  vars <- Monad.forM fields (\_ -> variable Flexible)
              fieldCons <-
                  Monad.zipWithM
                      (constrain env)
                      (map snd fields)
                      (map varN vars)
              let fields' = ST.fieldMap (zip (map fst fields) (map varN vars))
              let recordType = record fields' (termN EmptyRecord1)
              return (ex vars (CAnd (fieldCons ++ [tipe === recordType])))

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

      E.Port impl ->
          case impl of
            E.In _ _ ->
                return CTrue

            E.Out _ expr _ ->
                constrain env expr tipe

            E.Task _ expr _ ->
                constrain env expr tipe


-- CONSTRAIN BINOP

constrainBinop
    :: Env.Environment
    -> R.Region
    -> V.Canonical
    -> Canonical.Expr
    -> Canonical.Expr
    -> Type
    -> IO TypeConstraint
constrainBinop env region op leftExpr@(A.A leftRegion _) rightExpr@(A.A rightRegion _) tipe =
  do  leftVar <- variable Flexible
      rightVar <- variable Flexible

      leftCon <- constrain env leftExpr (varN leftVar)
      rightCon <- constrain env rightExpr (varN rightVar)

      leftVar' <- variable Flexible
      rightVar' <- variable Flexible
      answerVar <- variable Flexible

      let opType = varN leftVar' ==> varN rightVar' ==> varN answerVar

      return $
        ex [leftVar,rightVar,leftVar',rightVar',answerVar] $ CAnd $
          [ leftCon
          , rightCon
          , CInstance region (V.toString op) opType
          , CEqual (Error.BinopLeft op leftRegion) region (varN leftVar') (varN leftVar)
          , CEqual (Error.BinopRight op rightRegion) region (varN rightVar') (varN rightVar)
          , CEqual (Error.Binop op) region tipe (varN answerVar)
          ]


-- CONSTRAIN LISTS

constrainList
    :: Env.Environment
    -> R.Region
    -> [Canonical.Expr]
    -> Type
    -> IO TypeConstraint
constrainList env region exprs tipe =
  do  (exprInfo, exprCons) <-
          unzip <$> mapM elementConstraint exprs

      (vars, cons) <- pairCons region Error.ListElement varToCon exprInfo

      return $ ex vars (CAnd (exprCons ++ cons))
  where
    elementConstraint expr@(A.A region' _) =
      do  var <- variable Flexible
          con <- constrain env expr (varN var)
          return ( (var, region'), con )

    varToCon var =
      CEqual Error.List region tipe (Env.get env Env.types "List" <| varN var)


-- CONSTRAIN IF EXPRESSIONS

constrainIf
    :: Env.Environment
    -> R.Region
    -> [(Canonical.Expr, Canonical.Expr)]
    -> Type
    -> IO TypeConstraint
constrainIf env region branches tipe =
  do  (branchInfo, branchExprCons) <-
          unzip <$> mapM constrainBranch branches

      (vars,cons) <- branchCons branchInfo

      return $ ex vars (CAnd (branchExprCons ++ cons))
  where
    bool = Env.get env Env.types "Bool"

    constrainBranch (cond, expr@(A.A branchRegion _)) =
      do  branchVar <- variable Flexible
          condCon <- constrain env cond bool
          exprCon <- constrain env expr (varN branchVar)
          return
            ( (branchVar, branchRegion)
            , condCon /\ exprCon
            )

    branchCons branchInfo =
      case branchInfo of
        [(thenVar, _), (elseVar, _)] ->
            return
              ( [thenVar,elseVar]
              , [ CEqual Error.IfBranches region (varN thenVar) (varN elseVar)
                , varToCon thenVar
                ]
              )

        _ ->
            pairCons region Error.MultiIfBranch varToCon branchInfo

    varToCon var =
      CEqual Error.If region tipe (varN var)


-- CONSTRAIN CASE EXPRESSIONS

constrainCase
    :: Env.Environment
    -> R.Region
    -> Canonical.Expr
    -> [(P.CanonicalPattern, Canonical.Expr)]
    -> Type
    -> IO TypeConstraint
constrainCase env region expr branches tipe =
  do  exprVar <- variable Flexible
      exprCon <- constrain env expr (varN exprVar)

      (branchInfo, branchExprCons) <-
          unzip <$> mapM (branch (varN exprVar)) branches

      (vars, cons) <- pairCons region Error.CaseBranch varToCon branchInfo

      return $ ex (exprVar : vars) (CAnd (exprCon : branchExprCons ++ cons))
  where
    branch patternType (pattern, branchExpr@(A.A branchRegion _)) =
        do  branchVar <- variable Flexible
            fragment <- Pattern.constrain env pattern patternType
            branchCon <- constrain env branchExpr (varN branchVar)
            return
                ( (branchVar, branchRegion)
                , CLet [Fragment.toScheme fragment] branchCon
                )

    varToCon var =
      CEqual Error.Case region tipe (varN var)


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
      CEqual (pairHint index subregion) region (varN var1) (varN var2)
  in
  case collectPairs 2 items of
    Nothing ->
        do  var <- variable Flexible
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
expandPattern def@(Canonical.Definition lpattern expr maybeType) =
  let (A.A patternRegion pattern) = lpattern
  in
  case pattern of
    P.Var _ ->
        [def]

    _ ->
        mainDef : map toDef vars
      where
        vars = P.boundVarList lpattern

        combinedName = "$" ++ concat vars

        pvar name =
            A.A patternRegion (P.Var name)

        localVar name =
            A.A patternRegion (E.localVar name)

        mainDef = Canonical.Definition (pvar combinedName) expr maybeType

        toDef name =
            let extract =
                  E.Case (localVar combinedName) [(lpattern, localVar name)]
            in
                Canonical.Definition (pvar name) (A.A patternRegion extract) Nothing


-- CONSTRAIN DEFINITIONS

data Info = Info
    { iSchemes :: [TypeScheme]
    , iRigid :: [Variable]
    , iFlex :: [Variable]
    , iHeaders :: Map.Map String (A.Located Type)
    , iC2 :: TypeConstraint
    , iC1 :: TypeConstraint
    }


constrainDef :: Env.Environment -> Info -> Canonical.Def -> IO Info
constrainDef env info (Canonical.Definition (A.A patternRegion pattern) expr maybeTipe) =
  let qs = [] -- should come from the def, but I'm not sure what would live there...
  in
  case (pattern, maybeTipe) of
    (P.Var name, Just (A.A typeRegion tipe)) ->
        constrainAnnotatedDef env info qs patternRegion typeRegion name expr tipe

    (P.Var name, Nothing) ->
        constrainUnannotatedDef env info qs patternRegion name expr

    _ -> error ("problem in constrainDef with " ++ show pattern)


constrainAnnotatedDef
    :: Env.Environment
    -> Info
    -> [String]
    -> R.Region
    -> R.Region
    -> String
    -> Canonical.Expr
    -> ST.Canonical
    -> IO Info
constrainAnnotatedDef env info qs patternRegion typeRegion name expr tipe =
  do  -- Some mistake may be happening here. Currently, qs is always [].
      rigidVars <- Monad.forM qs (\_ -> variable Rigid)

      flexiVars <- Monad.forM qs (\_ -> variable Flexible)

      let inserts = zipWith (\arg typ -> Map.insert arg (varN typ)) qs flexiVars

      let env' = env { Env.value = List.foldl' (\x f -> f x) (Env.value env) inserts }

      (vars, typ) <- Env.instantiateType env tipe Map.empty

      let scheme =
            Scheme
            { rigidQuantifiers = []
            , flexibleQuantifiers = flexiVars ++ vars
            , constraint = CTrue
            , header = Map.singleton name (A.A patternRegion typ)
            }

      var <- variable Flexible
      defCon <- constrain env' expr (varN var)
      let annCon =
            CEqual (Error.BadTypeAnnotation name) typeRegion typ (varN var)

      return $ info
          { iSchemes = scheme : iSchemes info
          , iC1 = iC1 info /\ ex [var] (defCon /\ fl rigidVars annCon)
          }


constrainUnannotatedDef
    :: Env.Environment
    -> Info
    -> [String]
    -> R.Region
    -> String
    -> Canonical.Expr
    -> IO Info
constrainUnannotatedDef env info qs patternRegion name expr =
  do  -- Some mistake may be happening here. Currently, qs is always [].
      rigidVars <- Monad.forM qs (\_ -> variable Rigid)

      v <- variable Flexible

      let tipe = varN v

      let inserts = zipWith (\arg typ -> Map.insert arg (varN typ)) qs rigidVars

      let env' = env { Env.value = List.foldl' (\x f -> f x) (Env.value env) inserts }

      con <- constrain env' expr tipe

      return $ info
          { iRigid = rigidVars ++ iRigid info
          , iFlex = v : iFlex info
          , iHeaders = Map.insert name (A.A patternRegion tipe) (iHeaders info)
          , iC2 = con /\ iC2 info
          }
