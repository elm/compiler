{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Expression
  ( constrain
  )
  where


import Control.Arrow (second)
import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Shader as Shader
import qualified Data.Bag as Bag
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Region as R
import Type.Constraint
import qualified Type.Constrain.Pattern as Pattern
import Type.Type as Type hiding (Descriptor(..))



-- CONSTRAIN


-- The environment contains all known type variables.
--
-- So if you have a top-level type annotation like (func : a -> b) then
-- the environment will hold variables for `a` and `b`
--
type Env = Map.Map N.Name Variable


constrain :: Env -> R.Region -> Error.Context -> Can.Expr -> Type -> IO Constraint
constrain env parentRegion context (A.A region expression) tipe =
  case expression of
    Can.VarLocal name ->
      return (CLookup parentRegion context region name tipe)

    Can.VarTopLevel _ name ->
      return (CLookup parentRegion context region name tipe)

    Can.VarKernel _ _ ->
      return CTrue

    Can.VarForeign _ name annotation ->
      error "TODO figure out foreign types"

    Can.VarOperator op _ _ annotation ->
      error "TODO figure out foreign types"

    Can.Str _ ->
      return $ CEqual parentRegion context Error.String region Type.string tipe

    Can.Chr _ ->
      return $ CEqual parentRegion context Error.Char region Type.char tipe

    Can.Int _ ->
      do  var <- mkFlexNumber
          return $ ex [var] $
            CEqual parentRegion context Error.Number region (VarN var) tipe

    Can.Float _ ->
      return $ CEqual parentRegion context Error.Float region Type.float tipe

    Can.List elements ->
      constrainList env parentRegion context region elements tipe

    Can.Negate expr ->
      do  numberVar <- mkFlexNumber
          let numberType = VarN numberVar
          numberCon <- constrain env region Error.Negate expr numberType
          let negateCon = CEqual parentRegion context Error.Number region numberType tipe
          return $ ex [numberVar] $ CAnd [ numberCon, negateCon ]

    Can.Binop op home name annotation leftExpr rightExpr ->
      constrainBinop env parentRegion context region op annotation leftExpr rightExpr tipe

    Can.Lambda args body ->
      constrainLambda parentRegion context env region args body tipe

    Can.Call func args ->
      constrainCall env parentRegion context region func args tipe

    Can.If branches finally ->
      constrainIf env parentRegion context region branches finally tipe

    Can.Case expr branches ->
      constrainCase env parentRegion context region expr branches tipe

    Can.Let def body ->
      constrainLet env parentRegion context region def body tipe

    Can.LetRec defs body ->
      error "TODO let rec" defs body

    Can.LetDestruct pattern destructors expr body ->
      error "TODO let destruct" pattern destructors expr body

    Can.Accessor field ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType
          return $ ex [ fieldVar, extVar ] $
            CEqual parentRegion context (Error.Accessor field) region (FunN recordType fieldType) tipe

    Can.Access expr field ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType
          recordCon <- constrain env region (Error.RecordAccess field) expr recordType
          return $ ex [ fieldVar, extVar ] $
            CAnd
              [ recordCon
              , CEqual parentRegion context (Error.Access field) region recordType tipe
              ]

    Can.Update expr fields ->
      constrainUpdate env parentRegion context region expr fields tipe

    Can.Record fields ->
      constrainRecord env parentRegion context region fields tipe

    Can.Unit ->
      return $ CEqual parentRegion context Error.Unit region UnitN tipe

    Can.Tuple a b maybeC ->
      constrainTuple env parentRegion context region a b maybeC tipe

    Can.Shader _uid _src glType ->
      constrainShader env parentRegion context region glType tipe



-- CONSTRAIN LAMBDA


constrainLambda :: R.Region -> Error.Context -> Env -> R.Region -> Can.Args -> Can.Expr -> Type -> IO Constraint
constrainLambda parentRegion context env lambdaRegion (Can.Args args _) body tipe =
  constrainLambdaHelp parentRegion context env lambdaRegion args body [] Pattern.emptyState tipe


constrainLambdaHelp :: R.Region -> Error.Context -> Env -> R.Region -> [Can.Arg] -> Can.Expr -> [Type] -> Pattern.State -> Type -> IO Constraint
constrainLambdaHelp parentRegion context env lambdaRegion args body revArgTypes state tipe =
  case args of
    (Can.Arg index pattern) : otherArgs ->
      do  argVar <- mkFlexVar
          let argType = VarN argVar
          let newTypes = argType : revArgTypes
          let lambdaContext = Error.PatternArg Error.UnknownCall index
          newState <- Pattern.addConstraints lambdaRegion lambdaContext pattern argType state
          constrainLambdaHelp parentRegion context env lambdaRegion otherArgs body newTypes newState tipe

    [] ->
      do  resultVar <- mkFlexVar
          let resultType = VarN resultVar
          let lambdaType = foldr (==>) resultType revArgTypes
          let lambdaCon = CEqual parentRegion context Error.Lambda lambdaRegion lambdaType tipe
          bodyCon <- constrain env lambdaRegion Error.LambdaBody body resultType

          let (Pattern.State headers vars revCons) = state
          let scheme = Scheme [] (resultVar : Bag.toList vars) headers (CAnd (reverse revCons))

          return $ CLet [scheme] $ CAnd [ bodyCon, lambdaCon ]



-- CONSTRAIN CALL


constrainCall :: Env -> R.Region -> Error.Context -> R.Region -> Can.Expr -> [Can.Expr] -> Type -> IO Constraint
constrainCall env parentRegion context callRegion func args tipe =
  do  let callName = toCallName func
      let (A.A lastArgRegion _) = last args

      funcVar <- mkFlexVar
      let funcType = VarN funcVar
      funcCon <- constrain env callRegion Error.Whatever func funcType

      (vars, revCons, resultType) <-
        constrainCallHelp env callRegion callName lastArgRegion args Index.first [funcVar] funcType [funcCon]

      let callCon = CEqual parentRegion context (Error.CallResult callName) callRegion resultType tipe

      return $ ex vars $ CAnd (reverse (callCon:revCons))


constrainCallHelp
  :: Env
  -> R.Region
  -> Error.CallName
  -> R.Region
  -> [Can.Expr]
  -> Index.ZeroBased
  -> [Variable]
  -> Type
  -> [Constraint]
  -> IO ([Variable], [Constraint], Type)
constrainCallHelp env callRegion callName lastArgRegion args index vars funcType revCons =
  case args of
    [] ->
      return (vars, revCons, funcType)

    arg@(A.A argRegion _) : otherArgs ->
      do  argVar <- mkFlexVar
          resultVar <- mkFlexVar
          let argType = VarN argVar
          let resultType = VarN resultVar

          -- check arity
          let aRegion = R.merge argRegion lastArgRegion
          let aContext = Error.CallArity callName index
          let aCategory = Error.Call callName
          let arityCon = CEqual callRegion aContext aCategory aRegion funcType (argType ==> resultType)

          -- check arg type
          argCon <- constrain env callRegion (Error.CallArg callName index) arg argType

          let newIndex = Index.next index
          let newVars = argVar : resultVar : vars
          let newCons = argCon : arityCon : revCons
          constrainCallHelp env callRegion callName lastArgRegion otherArgs newIndex newVars resultType newCons


toCallName :: Can.Expr -> Error.CallName
toCallName (A.A _ expr) =
  case expr of
    Can.VarLocal name ->
      Error.FuncCall name

    Can.VarTopLevel _ name ->
      Error.FuncCall name

    Can.VarKernel _ name ->
      Error.FuncCall name

    Can.VarForeign _ name _ ->
      Error.FuncCall name

    Can.VarOperator op _ _ _ ->
      Error.OpCall op

    Can.Accessor field ->
      Error.AccessCall field

    _ ->
      Error.UnknownCall



-- CONSTRAIN BINOP


constrainBinop
    :: Env
    -> R.Region
    -> Error.Context
    -> R.Region
    -> N.Name
    -> Can.Annotation
    -> Can.Expr
    -> Can.Expr
    -> Type
    -> IO Constraint
constrainBinop env parentRegion context opRegion op srcType leftExpr rightExpr tipe =
  do  leftVar <- mkFlexVar
      rightVar <- mkFlexVar
      answerVar <- mkFlexVar
      let leftType = VarN leftVar
      let rightType = VarN rightVar
      let answerType = VarN answerVar

      let opCon = error "TODO CInstance" srcType (leftType ==> rightType ==> answerType)

      leftCon <- constrain env opRegion (Error.OpLeft op) leftExpr leftType
      rightCon <- constrain env opRegion (Error.OpRight op) rightExpr rightType

      return $ ex [ leftVar, rightVar, answerVar ] $
        CAnd
          [ opCon
          , leftCon
          , rightCon
          , CEqual parentRegion context (Error.Call (Error.OpCall op)) opRegion answerType tipe
          ]



-- CONSTRAIN LISTS


constrainList :: Env -> R.Region -> Error.Context -> R.Region -> [Can.Expr] -> Type -> IO Constraint
constrainList env parentRegion context listRegion entries tipe =
  do  entryVar <- mkFlexVar
      let entryType = VarN entryVar
      entryCons <- Index.indexedTraverse (constrainListEntry env listRegion entryType) entries
      let listType = AppN ModuleName.list N.list [entryType]
      return $ ex [entryVar] $
        CAnd
          [ CAnd entryCons
          , CEqual parentRegion context Error.List listRegion listType tipe
          ]


constrainListEntry :: Env -> R.Region -> Type -> Index.ZeroBased -> Can.Expr -> IO Constraint
constrainListEntry env listRegion tipe index expr =
  constrain env listRegion (Error.ListEntry index) expr tipe




-- CONSTRAIN IF EXPRESSIONS


constrainIf :: Env -> R.Region -> Error.Context -> R.Region -> [(Can.Expr, Can.Expr)] -> Can.Expr -> Type -> IO Constraint
constrainIf env parentRegion context ifRegion branches final tipe =
  do  branchVar <- mkFlexVar
      let branchType = VarN branchVar

      branchCons <- Index.indexedTraverse (constrainIfBranch env ifRegion branchType) branches
      finalCon <- constrain env ifRegion (Error.IfBranchFinal (length branches)) final branchType

      return $ ex [branchVar] $
        CAnd
          [ CAnd branchCons
          , finalCon
          , CEqual parentRegion context Error.If ifRegion branchType tipe
          ]


constrainIfBranch :: Env -> R.Region -> Type -> Index.ZeroBased -> (Can.Expr, Can.Expr) -> IO Constraint
constrainIfBranch env ifRegion branchType index (condition, branch) =
  do  condCon <- constrain env ifRegion Error.IfCondition condition Type.bool
      branchCon <- constrain env ifRegion (Error.IfBranch index) branch branchType
      return $ CAnd [ condCon, branchCon ]


-- CONSTRAIN CASE EXPRESSIONS


constrainCase :: Env -> R.Region -> Error.Context -> R.Region -> Can.Expr -> [Can.CaseBranch] -> Type -> IO Constraint
constrainCase env parentRegion context caseRegion expr branches tipe =
  do  exprVar <- mkFlexVar
      branchVar <- mkFlexVar
      let exprType = VarN exprVar
      let branchType = VarN branchVar

      exprCon <- constrain env caseRegion Error.CaseExpr expr exprType
      revCons <- constrainCaseHelp env caseRegion branches exprType branchType [exprCon]
      let caseCon = CEqual parentRegion context Error.Case caseRegion branchType tipe

      return $ ex [ exprVar, branchVar ] $ CAnd (reverse (caseCon:revCons))



constrainCaseHelp :: Env -> R.Region -> [Can.CaseBranch] -> Type -> Type -> [Constraint] -> IO [Constraint]
constrainCaseHelp env caseRegion branches patternType exprType revCons =
  case branches of
    [] ->
      return revCons

    (Can.CaseBranch index pattern destructors expr) : otherBranches ->
      do  (Pattern.State headers vars revCons) <-
            Pattern.addConstraints caseRegion (Error.PatternCaseMatch index) pattern patternType Pattern.emptyState

          exprCon <- constrain env caseRegion (Error.CaseBranch index) expr exprType
          let branchCon = CLet [ Scheme [] (Bag.toList vars) headers (CAnd (reverse revCons)) ] exprCon
          constrainCaseHelp env caseRegion otherBranches patternType exprType (branchCon:revCons)



-- CONSTRAIN RECORD


constrainRecord :: Env -> R.Region -> Error.Context -> R.Region -> Map.Map N.Name Can.Expr -> Type -> IO Constraint
constrainRecord env parentRegion context recordRegion fields tipe =
  do  dict <- Map.traverseWithKey (constrainField env recordRegion) fields

      let getType (_, t, _) = t
      let recordType = RecordN (Map.map getType dict) EmptyRecordN
      let recordCon = CEqual parentRegion context Error.Record recordRegion recordType tipe

      let vars = Map.foldr (\(v,_,_) vs -> v:vs) [] dict
      let cons = Map.foldr (\(_,_,c) cs -> c:cs) [recordCon] dict

      return $ ex vars (CAnd cons)


constrainField :: Env -> R.Region -> N.Name -> Can.Expr -> IO (Variable, Type, Constraint)
constrainField env recordRegion name expr =
  do  var <- mkFlexVar
      let tipe = VarN var
      con <- constrain env recordRegion (Error.RecordField name) expr tipe
      return (var, tipe, con)



-- CONSTRAIN RECORD UPDATE


constrainUpdate :: Env -> R.Region -> Error.Context -> R.Region -> Can.Expr -> Map.Map N.Name Can.Expr -> Type -> IO Constraint
constrainUpdate env parentRegion context recordRegion expr fields tipe =
  do  sharedVar <- mkFlexVar
      let sharedType = VarN sharedVar

      oldVars <- traverse (\_ -> mkFlexVar) fields
      let oldTypes = Map.map VarN oldVars
      let oldRecordType = RecordN oldTypes sharedType
      oldCon <- constrain env recordRegion Error.RecordUpdate expr oldRecordType

      newDict <- Map.traverseWithKey (constrainField env recordRegion) fields

      let getType (_, t, _) = t
      let newRecordType = RecordN (Map.map getType newDict) sharedType
      let newCon = CEqual parentRegion context Error.Record recordRegion newRecordType tipe

      let vars = Map.foldr (\(v,_,_) vs -> v:vs) (sharedVar : Map.elems oldVars) newDict
      let cons = Map.foldr (\(_,_,c) cs -> c:cs) [newCon] newDict

      return $ ex vars (CAnd (oldCon:cons))



-- CONSTRAIN TUPLE


constrainTuple :: Env -> R.Region -> Error.Context -> R.Region -> Can.Expr -> Can.Expr -> Maybe Can.Expr -> Type -> IO Constraint
constrainTuple env parentRegion context tupleRegion a b maybeC tipe =
  do  aVar <- mkFlexVar
      bVar <- mkFlexVar
      let aType = VarN aVar
      let bType = VarN bVar

      aCon <- constrain env tupleRegion Error.Whatever a aType
      bCon <- constrain env tupleRegion Error.Whatever b bType

      case maybeC of
        Nothing ->
          do  let tupleType = TupleN aType bType Nothing
              let tupleCon = CEqual parentRegion context Error.Tuple tupleRegion tupleType tipe
              return $ ex [ aVar, bVar ] $ CAnd [ aCon, bCon, tupleCon ]

        Just c ->
          do  cVar <- mkFlexVar
              let cType = VarN cVar

              cCon <- constrain env tupleRegion Error.Whatever c cType

              let tupleType = TupleN aType bType (Just cType)
              let tupleCon = CEqual parentRegion context Error.Tuple tupleRegion tupleType tipe

              return $ ex [ aVar, bVar, cVar ] $ CAnd [ aCon, bCon, cCon, tupleCon ]



-- CONSTRAIN SHADER


constrainShader :: Env -> R.Region -> Error.Context -> R.Region -> Shader.Shader -> Type -> IO Constraint
constrainShader env parentRegion context shaderRegion (Shader.Shader attributes uniforms varyings) tipe =
  do  attrVar <- mkFlexVar
      unifVar <- mkFlexVar
      let attrType = VarN attrVar
      let unifType = VarN unifVar

      let shaderType =
            AppN ModuleName.webgl N.shader
              [ toShaderRecord attributes attrType
              , toShaderRecord uniforms unifType
              , toShaderRecord varyings EmptyRecordN
              ]

      return $ ex [ attrVar, unifVar ] $
        CEqual parentRegion context Error.Shader shaderRegion shaderType tipe


toShaderRecord :: Map.Map N.Name Shader.Type -> Type -> Type
toShaderRecord types baseRecType =
  if Map.null types then
    baseRecType
  else
    RecordN (Map.map glToType types) baseRecType


glToType :: Shader.Type -> Type
glToType glType =
  case glType of
    Shader.V2 -> Type.vec2
    Shader.V3 -> Type.vec3
    Shader.V4 -> Type.vec4
    Shader.M4 -> Type.mat4
    Shader.Int -> Type.int
    Shader.Float -> Type.float
    Shader.Texture -> Type.texture



-- CONSTRAIN LET


constrainLet :: Env -> R.Region -> Error.Context -> R.Region -> Can.Def -> Can.Expr -> Type -> IO Constraint
constrainLet env parentRegion context letRegion (Can.Def (A.A _ name) (Can.Args args _) expr maybeType) body tipe =
  case maybeType of
    Nothing ->
      error "TODO"

    Just srcType ->
      error "TODO"
