{-# OPTIONS_GHC -Wall #-}
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
import qualified Type.Instantiate as Instantiate
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
          return $ exists [var] $ CEqual region Number (VarN var) expectation

    Can.Float _ ->
      return $ CEqual region Float Type.float expectation

    Can.List elements ->
      constrainList rtv region elements expectation

    Can.Negate expr ->
      do  numberVar <- mkFlexNumber
          let numberType = VarN numberVar
          numberCon <- constrain rtv expr (FromContext region Negate numberType)
          let negateCon = CEqual region Number numberType expectation
          return $ exists [numberVar] $ CAnd [ numberCon, negateCon ]

    Can.Binop op _ _ annotation leftExpr rightExpr ->
      constrainBinop rtv region op annotation leftExpr rightExpr expectation

    Can.Lambda args _ body ->
      constrainLambda rtv region args body expectation

    Can.Call func args ->
      constrainCall rtv region func args expectation

    Can.If branches finally ->
      constrainIf rtv region branches finally expectation

    Can.Case expr branches ->
      constrainCase rtv region expr branches expectation

    Can.Let def body ->
      constrainDef rtv def
      =<< constrain rtv body expectation

    Can.LetRec defs body ->
      constrainRecursiveDefs rtv defs
      =<< constrain rtv body expectation

    Can.LetDestruct pattern _ expr body ->
      constrainDestruct rtv region pattern expr
      =<< constrain rtv body expectation

    Can.Accessor field ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType
          return $ exists [ fieldVar, extVar ] $
            CEqual region (Accessor field) (FunN recordType fieldType) expectation

    Can.Access expr field ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType

          recordCon <-
            constrain rtv expr (FromContext region (RecordAccess field) recordType)

          return $ exists [ fieldVar, extVar ] $
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



-- CONSTRAIN LAMBDA


constrainLambda :: RTV -> R.Region -> [Can.Arg] -> Can.Expr -> Expectation -> IO Constraint
constrainLambda rtv region args body expectation =
  do  (Args vars tipe resultType (Pattern.State headers pvars revCons)) <-
        constrainArgs args

      bodyCon <-
        constrain rtv body (NoExpectation resultType)

      return $ exists vars $
        CAnd
          [ CLet
              { _rigidVars = []
              , _flexVars = pvars
              , _header = headers
              , _headerCon = CAnd (reverse revCons)
              , _bodyCon = bodyCon
              }
          , CEqual region Lambda tipe expectation
          ]



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

      return $ exists (funcVar:resultVar:argVars) $
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

      return $ exists (resultVar:argVars) $ CAnd [ CAnd argCons, funcCon ]


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

      return $ exists [ leftVar, rightVar, answerVar ] $
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

      return $ exists [entryVar] $
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

              return $ exists [branchVar] $
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

              return $ exists [ptrnVar] $ CAnd (exprCon:branchCons)

        _ ->
          do  branchVar <- mkFlexVar
              let branchType = VarN branchVar

              branchCons <- Index.indexedForA branches $ \index branch ->
                constrainCaseBranch rtv branch
                  (PatternExpectation region (PCaseMatch index) ptrnType)
                  (FromContext region (CaseBranch index) branchType)

              return $ exists [ptrnVar,branchVar] $
                CAnd
                  [ exprCon
                  , CAnd branchCons
                  , CEqual region Case branchType expectation
                  ]


constrainCaseBranch :: RTV -> Can.CaseBranch -> PatternExpectation -> Expectation -> IO Constraint
constrainCaseBranch rtv (Can.CaseBranch pattern _ expr) pExpect bExpect =
  do  (Pattern.State headers pvars revCons) <-
        Pattern.add pattern pExpect Pattern.emptyState

      CLet [] pvars headers (CAnd (reverse revCons))
        <$> constrain rtv expr bExpect



-- CONSTRAIN RECORD


constrainRecord :: RTV -> R.Region -> Map.Map N.Name Can.Expr -> Expectation -> IO Constraint
constrainRecord rtv region fields expectation =
  do  dict <- traverse (constrainField rtv) fields

      let getType (_, t, _) = t
      let recordType = RecordN (Map.map getType dict) EmptyRecordN
      let recordCon = CEqual region Record recordType expectation

      let vars = Map.foldr (\(v,_,_) vs -> v:vs) [] dict
      let cons = Map.foldr (\(_,_,c) cs -> c:cs) [recordCon] dict

      return $ exists vars (CAnd cons)


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

      return $ exists vars (CAnd (oldCon:cons))



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
              return $ exists [ aVar, bVar ] $ CAnd [ aCon, bCon, tupleCon ]

        Just c ->
          do  cVar <- mkFlexVar
              let cType = VarN cVar

              cCon <- constrain rtv c (NoExpectation cType)

              let tupleType = TupleN aType bType (Just cType)
              let tupleCon = CEqual region Tuple tupleType expectation

              return $ exists [ aVar, bVar, cVar ] $ CAnd [ aCon, bCon, cCon, tupleCon ]



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

      return $ exists [ attrVar, unifVar ] $
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



-- CONSTRAIN DESTRUCTURES


constrainDestruct :: RTV -> R.Region -> Can.Pattern -> Can.Expr -> Constraint -> IO Constraint
constrainDestruct rtv region pattern expr bodyCon =
  do  patternVar <- mkFlexVar
      let patternType = VarN patternVar

      (Pattern.State headers pvars revCons) <-
        Pattern.add pattern (NoPatternExpectation patternType) Pattern.emptyState

      exprCon <-
        constrain rtv expr (FromContext region Destructure patternType)

      return $ CLet [] pvars headers (CAnd (reverse (exprCon:revCons))) bodyCon



-- CONSTRAIN DEF


constrainDef :: RTV -> Can.Def -> Constraint -> IO Constraint
constrainDef rtv def bodyCon =
  case def of
    Can.Def (A.A region name) args _ expr ->
      do  (Args vars tipe resultType (Pattern.State headers pvars revCons)) <-
            constrainArgs args

          exprCon <-
            constrain rtv expr (NoExpectation resultType)

          return $
            CLet
              { _rigidVars = []
              , _flexVars = vars
              , _header = Map.singleton name (A.A region tipe)
              , _headerCon =
                  CLet
                    { _rigidVars = []
                    , _flexVars = pvars
                    , _header = headers
                    , _headerCon = CAnd (reverse revCons)
                    , _bodyCon = exprCon
                    }
              , _bodyCon = bodyCon
              }

    Can.TypedDef (A.A region name) freeVars typedArgs _ expr srcResultType ->
      do  let newNames = Map.difference freeVars rtv
          newRigids <- Map.traverseWithKey (\n _ -> nameToRigid n) newNames
          let newRtv = Map.union rtv (Map.map VarN newRigids)

          (TypedArgs tipe resultType (Pattern.State headers pvars revCons)) <-
            constrainTypedArgs newRtv name typedArgs srcResultType

          let expectation = FromAnnotation name (length typedArgs) TypedBody resultType
          exprCon <-
            constrain newRtv expr expectation

          return $
            CLet
              { _rigidVars = Map.elems newRigids
              , _flexVars = []
              , _header = Map.singleton name (A.A region tipe)
              , _headerCon =
                  CLet
                    { _rigidVars = []
                    , _flexVars = pvars
                    , _header = headers
                    , _headerCon = CAnd (reverse revCons)
                    , _bodyCon = exprCon
                    }
              , _bodyCon = bodyCon
              }



-- CONSTRAIN RECURSIVE DEFS


data Info =
  Info
    { _vars :: [Variable]
    , _cons :: [Constraint]
    , _headers :: Map.Map N.Name (A.Located Type)
    }


{-# NOINLINE emptyInfo #-}
emptyInfo :: Info
emptyInfo =
  Info [] [] Map.empty


constrainRecursiveDefs :: RTV -> [Can.Def] -> Constraint -> IO Constraint
constrainRecursiveDefs rtv defs bodyCon =
  recDefsHelp rtv defs bodyCon emptyInfo emptyInfo


recDefsHelp :: RTV -> [Can.Def] -> Constraint -> Info -> Info -> IO Constraint
recDefsHelp rtv defs bodyCon rigidInfo flexInfo =
  case defs of
    [] ->
      do  let (Info rigidVars rigidCons rigidHeaders) = rigidInfo
          let (Info flexVars  flexCons  flexHeaders ) = flexInfo
          return $
            CLet rigidVars [] rigidHeaders CTrue $
              CLet [] flexVars flexHeaders (CLet [] [] flexHeaders CTrue (CAnd flexCons)) $
                CAnd [ CAnd rigidCons, bodyCon ]

    def : otherDefs ->
      case def of
        Can.Def (A.A region name) args _ expr ->
          do  let (Info flexVars flexCons flexHeaders) = flexInfo

              (Args newFlexVars tipe resultType (Pattern.State headers pvars revCons)) <-
                argsHelp args (Pattern.State Map.empty flexVars [])

              exprCon <-
                constrain rtv expr (NoExpectation resultType)

              let defCon =
                    CLet
                      { _rigidVars = []
                      , _flexVars = pvars
                      , _header = headers
                      , _headerCon = CAnd (reverse revCons)
                      , _bodyCon = exprCon
                      }

              recDefsHelp rtv otherDefs bodyCon rigidInfo $
                Info
                  { _vars = newFlexVars
                  , _cons = defCon : flexCons
                  , _headers = Map.insert name (A.A region tipe) flexHeaders
                  }

        Can.TypedDef (A.A region name) freeVars typedArgs _ expr srcResultType ->
          do  let newNames = Map.difference freeVars rtv
              newRigids <- Map.traverseWithKey (\n _ -> nameToRigid n) newNames
              let newRtv = Map.union rtv (Map.map VarN newRigids)

              (TypedArgs tipe resultType (Pattern.State headers pvars revCons)) <-
                constrainTypedArgs newRtv name typedArgs srcResultType

              exprCon <-
                constrain newRtv expr $
                  FromAnnotation name (length typedArgs) TypedBody resultType

              let defCon =
                    CLet
                      { _rigidVars = []
                      , _flexVars = pvars
                      , _header = headers
                      , _headerCon = CAnd (reverse revCons)
                      , _bodyCon = exprCon
                      }

              let (Info rigidVars rigidCons rigidHeaders) = rigidInfo
              recDefsHelp rtv otherDefs bodyCon
                ( Info
                    { _vars = Map.foldr (:) rigidVars newRigids
                    , _cons = defCon : rigidCons
                    , _headers = Map.insert name (A.A region tipe) rigidHeaders
                    }
                )
                flexInfo



-- CONSTRAIN ARGS


data Args =
  Args
    { _a_vars :: [Variable]
    , _a_type :: Type
    , _a_result :: Type
    , _a_state :: Pattern.State
    }


constrainArgs :: [Can.Arg] -> IO Args
constrainArgs args =
  argsHelp args Pattern.emptyState


argsHelp :: [Can.Arg] -> Pattern.State -> IO Args
argsHelp args state =
  case args of
    [] ->
      do  resultVar <- mkFlexVar
          let resultType = VarN resultVar
          return $ Args [resultVar] resultType resultType state

    (Can.Arg _ pattern) : otherArgs ->
      do  argVar <- mkFlexVar
          let argType = VarN argVar

          (Args vars tipe result newState) <-
            argsHelp otherArgs =<<
              Pattern.add pattern (NoPatternExpectation argType) state

          return (Args (argVar:vars) (FunN argType tipe) result newState)



-- CONSTRAIN TYPED ARGS


data TypedArgs =
  TypedArgs
    { _t_type :: Type
    , _t_result :: Type
    , _t_state :: Pattern.State
    }


constrainTypedArgs :: Map.Map N.Name Type -> N.Name -> [Can.TypedArg] -> Can.Type -> IO TypedArgs
constrainTypedArgs rtv name args srcResultType =
  do  resultType <- Instantiate.fromSrcType rtv srcResultType
      typedArgsHelp rtv name args resultType Pattern.emptyState


typedArgsHelp :: Map.Map N.Name Type -> N.Name -> [Can.TypedArg] -> Type -> Pattern.State -> IO TypedArgs
typedArgsHelp rtv name args resultType state =
  case args of
    [] ->
      return $ TypedArgs resultType resultType state

    (Can.TypedArg index srcType pattern@(A.A region _)) : otherArgs ->
      do  argType <- Instantiate.fromSrcType rtv srcType
          let expect = PatternExpectation region (PTypedArg name index) argType

          (TypedArgs tipe result newState) <-
            typedArgsHelp rtv name otherArgs resultType =<<
              Pattern.add pattern expect state

          return (TypedArgs (FunN argType tipe) result newState)
