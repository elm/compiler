{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Constrain.Expression
  ( constrain
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R
import Type.Constraint
import qualified Type.Constrain.Pattern as Pattern
import Type.Type as Type hiding (Descriptor(..))



-- CONSTRAIN


-- As we step past type annotations, the free type variables are added to
-- the "rigid type variables" dict. Allowing sharing of rigid variables
-- between nested type annotations.
--
-- So if you have a top-level type annotation like (func : a -> b) the RTV
-- dictionary will hold variables for `a` and `b`
--
type RTV =
  Map.Map N.Name Type


constrain :: RTV -> Can.Expr -> Expectation -> IO Constraint
constrain rtv (A.A region expression) expectation =
  case expression of
    Can.VarLocal name ->
      return (CLookup region name expectation)

    Can.VarTopLevel _ name ->
      return (CLookup region name expectation)

    Can.VarKernel _ _ ->
      return CTrue

    Can.VarForeign _ name annotation ->
      return $ CInstance region (FuncName name) annotation expectation

    Can.VarOperator op _ _ annotation ->
      return $ CInstance region (OpName op) annotation expectation

    Can.Str _ ->
      return $ CEqual region String Type.string expectation

    Can.Chr _ ->
      return $ CEqual region Char Type.char expectation

    Can.Int _ ->
      do  var <- mkFlexNumber
          return $ ex [var] $ CEqual region Number (VarN var) expectation

    Can.Float _ ->
      return $ CEqual region Float Type.float expectation

    Can.List elements ->
      constrainList rtv region elements expectation

    Can.Negate expr ->
      do  numberVar <- mkFlexNumber
          let numberType = VarN numberVar
          numberCon <- constrain rtv expr (FromContext region Negate numberType)
          let negateCon = CEqual region Number numberType expectation
          return $ ex [numberVar] $ CAnd [ numberCon, negateCon ]

    Can.Binop op _ _ annotation leftExpr rightExpr ->
      constrainBinop rtv region op annotation leftExpr rightExpr expectation

    Can.Lambda args _ body ->
      do  (lambdaType, resultType, scheme) <- Pattern.constrainArgs args
          bodyCon <- constrain rtv body (NoExpectation resultType)
          let lambdaCon = CEqual region Lambda lambdaType expectation
          return $ CLet [scheme] $ CAnd [ bodyCon, lambdaCon ]

    Can.Call func args ->
      constrainCall rtv region func args expectation

    Can.If branches finally ->
      constrainIf rtv region branches finally expectation

    Can.Case expr branches ->
      constrainCase rtv region expr branches expectation

    Can.Let def body ->
      constrainDef rtv def =<< constrain rtv body expectation

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
            CEqual region (Accessor field) (FunN recordType fieldType) expectation

    Can.Access expr field ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType

          recordCon <-
            constrain rtv expr (FromContext region (RecordAccess field) recordType)

          return $ ex [ fieldVar, extVar ] $
            CAnd
              [ recordCon
              , CEqual region (Access field) recordType expectation
              ]

    Can.Update expr fields ->
      constrainUpdate rtv region expr fields expectation

    Can.Record fields ->
      constrainRecord rtv region fields expectation

    Can.Unit ->
      return $ CEqual region Unit UnitN expectation

    Can.Tuple a b maybeC ->
      constrainTuple rtv region a b maybeC expectation

    Can.Shader _uid _src glType ->
      constrainShader region glType expectation



-- CONSTRAIN CALL


constrainCall :: RTV -> R.Region -> Can.Expr -> [Can.Expr] -> Expectation -> IO Constraint
constrainCall rtv region func args expectation =
  do  let maybeFuncName = getFuncName func

      funcVar <- mkFlexVar
      resultVar <- mkFlexVar
      let funcType = VarN funcVar
      let resultType = VarN resultVar

      funcCon <- constrain rtv func (NoExpectation funcType)

      (argVars, argTypes, argCons) <-
        unzip3 <$> Index.indexedTraverse (constrainArg rtv region maybeFuncName) args

      let arityType = foldr FunN resultType argTypes
      let resultCon = CEqual region (CallResult maybeFuncName) resultType expectation

      return $ ex (funcVar:resultVar:argVars) $
        CAnd
          [ funcCon
          , CBranch
              { _a = funcType
              , _b = arityType
              , _eq = CAnd [ CAnd argCons, resultCon ]
              , _neq = constrainCallBackup rtv region maybeFuncName func args
              }
          ]


constrainArg :: RTV -> R.Region -> Maybe FuncName -> Index.ZeroBased -> Can.Expr -> IO (Variable, Type, Constraint)
constrainArg rtv region maybeFuncName index arg =
  do  argVar <- mkFlexVar
      let argType = VarN argVar
      argCon <- constrain rtv arg (FromContext region (CallArg maybeFuncName index) argType)
      return (argVar, argType, argCon)


constrainCallBackup :: RTV -> R.Region -> Maybe FuncName -> Can.Expr -> [Can.Expr] -> IO Constraint
constrainCallBackup rtv region maybeFuncName func args =
  do  (argVars, argTypes, argCons) <-
        unzip3 <$> Index.indexedTraverse (constrainArg rtv region maybeFuncName) args

      resultVar <- mkFlexVar
      let resultType = VarN resultVar
      let arityType = foldr FunN resultType argTypes

      funcCon <- constrain rtv func (FromContext region BadArity arityType)

      return $ ex (resultVar:argVars) $ CAnd [ CAnd argCons, funcCon ]


getFuncName :: Can.Expr -> Maybe FuncName
getFuncName (A.A _ expr) =
  case expr of
    Can.VarLocal name ->
      Just (FuncName name)

    Can.VarTopLevel _ name ->
      Just (FuncName name)

    Can.VarKernel _ name ->
      Just (FuncName name)

    Can.VarForeign _ name _ ->
      Just (FuncName name)

    Can.VarOperator op _ _ _ ->
      Just (OpName op)

    _ ->
      Nothing



-- CONSTRAIN BINOP


constrainBinop :: RTV -> R.Region -> N.Name -> Can.Annotation -> Can.Expr -> Can.Expr -> Expectation -> IO Constraint
constrainBinop rtv region op annotation leftExpr rightExpr expectation =
  do  leftVar <- mkFlexVar
      rightVar <- mkFlexVar
      answerVar <- mkFlexVar
      let leftType = VarN leftVar
      let rightType = VarN rightVar
      let answerType = VarN answerVar
      let binopType = leftType ==> rightType ==> answerType

      let opCon = CInstance region (OpName op) annotation (NoExpectation binopType)

      leftCon <- constrain rtv leftExpr (FromContext region (OpLeft op) leftType)
      rightCon <- constrain rtv rightExpr (FromContext region (OpRight op) rightType)

      return $ ex [ leftVar, rightVar, answerVar ] $
        CAnd
          [ opCon
          , leftCon
          , rightCon
          , CEqual region (CallResult (Just (OpName op))) answerType expectation
          ]



-- CONSTRAIN LISTS


constrainList :: RTV -> R.Region -> [Can.Expr] -> Expectation -> IO Constraint
constrainList rtv region entries expectation =
  do  entryVar <- mkFlexVar
      let entryType = VarN entryVar
      let listType = AppN ModuleName.list N.list [entryType]

      entryCons <-
        Index.indexedTraverse (constrainListEntry rtv region entryType) entries

      return $ ex [entryVar] $
        CAnd
          [ CAnd entryCons
          , CEqual region List listType expectation
          ]


constrainListEntry :: RTV -> R.Region -> Type -> Index.ZeroBased -> Can.Expr -> IO Constraint
constrainListEntry rtv region tipe index expr =
  constrain rtv expr (FromContext region (ListEntry index) tipe)



-- CONSTRAIN IF EXPRESSIONS


constrainIf :: RTV -> R.Region -> [(Can.Expr, Can.Expr)] -> Can.Expr -> Expectation -> IO Constraint
constrainIf rtv region branches final expectation =
  do  let boolExpect = FromContext region IfCondition Type.bool
      let (conditions, exprs) = foldr (\(c,e) (cs,es) -> (c:cs,e:es)) ([],[final]) branches

      condCons <-
        traverse (\c -> constrain rtv c boolExpect) conditions

      case expectation of
        FromAnnotation name arity _ tipe ->
          do  branchCons <- Index.indexedForA exprs $ \index expr ->
                constrain rtv expr (FromAnnotation name arity (TypedIfBranch index) tipe)
              return $
                CAnd
                  [ CAnd condCons
                  , CAnd branchCons
                  ]

        _ ->
          do  branchVar <- mkFlexVar
              let branchType = VarN branchVar

              branchCons <- Index.indexedForA exprs $ \index expr ->
                constrain rtv expr (FromContext region (IfBranch index) branchType)

              return $ ex [branchVar] $
                CAnd
                  [ CAnd condCons
                  , CAnd branchCons
                  , CEqual region If branchType expectation
                  ]



-- CONSTRAIN CASE EXPRESSIONS


constrainCase :: RTV -> R.Region -> Can.Expr -> [Can.CaseBranch] -> Expectation -> IO Constraint
constrainCase rtv region expr branches expectation =
  do  ptrnVar <- mkFlexVar
      let ptrnType = VarN ptrnVar
      exprCon <- constrain rtv expr (NoExpectation ptrnType)

      case expectation of
        FromAnnotation name arity _ tipe ->
          do  branchCons <- Index.indexedForA branches $ \index branch ->
                constrainCaseBranch rtv branch
                  (PatternExpectation region (PCaseMatch index) ptrnType)
                  (FromAnnotation name arity (TypedCaseBranch index) tipe)

              return $ ex [ptrnVar] $ CAnd (exprCon:branchCons)

        _ ->
          do  branchVar <- mkFlexVar
              let branchType = VarN branchVar

              branchCons <- Index.indexedForA branches $ \index branch ->
                constrainCaseBranch rtv branch
                  (PatternExpectation region (PCaseMatch index) ptrnType)
                  (FromContext region (CaseBranch index) branchType)

              return $ ex [ptrnVar,branchVar] $
                CAnd
                  [ exprCon
                  , CAnd branchCons
                  , CEqual region Case branchType expectation
                  ]


constrainCaseBranch :: RTV -> Can.CaseBranch -> PatternExpectation -> Expectation -> IO Constraint
constrainCaseBranch rtv (Can.CaseBranch pattern _ expr) pExpect bExpect =
  do  scheme <- Pattern.constrain pattern pExpect
      constraint <- constrain rtv expr bExpect
      return $ CLet [scheme] constraint



-- CONSTRAIN RECORD


constrainRecord :: RTV -> R.Region -> Map.Map N.Name Can.Expr -> Expectation -> IO Constraint
constrainRecord rtv region fields expectation =
  do  dict <- traverse (constrainField rtv) fields

      let getType (_, t, _) = t
      let recordType = RecordN (Map.map getType dict) EmptyRecordN
      let recordCon = CEqual region Record recordType expectation

      let vars = Map.foldr (\(v,_,_) vs -> v:vs) [] dict
      let cons = Map.foldr (\(_,_,c) cs -> c:cs) [recordCon] dict

      return $ ex vars (CAnd cons)


constrainField :: RTV -> Can.Expr -> IO (Variable, Type, Constraint)
constrainField rtv expr =
  do  var <- mkFlexVar
      let tipe = VarN var
      con <- constrain rtv expr (NoExpectation tipe)
      return (var, tipe, con)



-- CONSTRAIN RECORD UPDATE


constrainUpdate :: RTV -> R.Region -> Can.Expr -> Map.Map N.Name Can.Expr -> Expectation -> IO Constraint
constrainUpdate rtv region expr fields expectation =
  do  sharedVar <- mkFlexVar
      let sharedType = VarN sharedVar

      oldVars <- traverse (\_ -> mkFlexVar) fields
      let oldTypes = Map.map VarN oldVars
      let oldRecordType = RecordN oldTypes sharedType
      oldCon <- constrain rtv expr (FromContext region RecordUpdate oldRecordType)

      newDict <- traverse (constrainField rtv) fields

      let getType (_, t, _) = t
      let newRecordType = RecordN (Map.map getType newDict) sharedType
      let newCon = CEqual region Record newRecordType expectation

      let vars = Map.foldr (\(v,_,_) vs -> v:vs) (sharedVar : Map.elems oldVars) newDict
      let cons = Map.foldr (\(_,_,c) cs -> c:cs) [newCon] newDict

      return $ ex vars (CAnd (oldCon:cons))



-- CONSTRAIN TUPLE


constrainTuple :: RTV -> R.Region -> Can.Expr -> Can.Expr -> Maybe Can.Expr -> Expectation -> IO Constraint
constrainTuple rtv region a b maybeC expectation =
  do  aVar <- mkFlexVar
      bVar <- mkFlexVar
      let aType = VarN aVar
      let bType = VarN bVar

      aCon <- constrain rtv a (NoExpectation aType)
      bCon <- constrain rtv b (NoExpectation bType)

      case maybeC of
        Nothing ->
          do  let tupleType = TupleN aType bType Nothing
              let tupleCon = CEqual region Tuple tupleType expectation
              return $ ex [ aVar, bVar ] $ CAnd [ aCon, bCon, tupleCon ]

        Just c ->
          do  cVar <- mkFlexVar
              let cType = VarN cVar

              cCon <- constrain rtv c (NoExpectation cType)

              let tupleType = TupleN aType bType (Just cType)
              let tupleCon = CEqual region Tuple tupleType expectation

              return $ ex [ aVar, bVar, cVar ] $ CAnd [ aCon, bCon, cCon, tupleCon ]



-- CONSTRAIN SHADER


constrainShader :: R.Region -> Shader.Shader -> Expectation -> IO Constraint
constrainShader region (Shader.Shader attributes uniforms varyings) expectation =
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
        CEqual region Shader shaderType expectation


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


constrainDef :: RTV -> Can.Def -> Constraint -> IO Constraint
constrainDef rtv def bodyCon =
  case def of
    Can.Def (A.A region name) args _ expr ->
      do  (lambdaType, resultType, scheme) <-
            Pattern.constrainArgs args

          defCon <-
            CLet [scheme] <$> constrain rtv expr (NoExpectation resultType)

          let headers = Map.singleton name (A.A region lambdaType)
          return $ CLet [Scheme [] [] headers defCon] bodyCon

    Can.TypedDef (A.A region name) freeVars typedArgs _ expr srcResultType ->
      do  let newNames = Map.difference freeVars rtv
          newRigids <- Map.traverseWithKey (\n _ -> nameToRigid n) newNames
          let newRtv = Map.union rtv (Map.map VarN newRigids)

          (lambdaType, resultType, scheme) <-
            Pattern.constrainTypedArgs newRtv typedArgs srcResultType

          let expectation = FromAnnotation name (length typedArgs) TypedBody resultType
          defCon <-
            CLet [scheme] <$> constrain newRtv expr expectation

          let headers = Map.singleton name (A.A region lambdaType)
          return $ CLet [ Scheme (Map.elems newRigids) [] headers defCon ] bodyCon
