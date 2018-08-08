{-# OPTIONS_GHC -Wall #-}
module Type.Constrain.Expression
  ( constrain
  , constrainDef
  , constrainRecursiveDefs
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import Reporting.Error.Type (Expected(..), Context(..), SubContext(..), MaybeName(..), Category(..), PExpected(..), PContext(..))
import qualified Reporting.Region as R
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


constrain :: RTV -> Can.Expr -> Expected Type -> IO Constraint
constrain rtv (A.At region expression) expected =
  case expression of
    Can.VarLocal name ->
      return (CLocal region name expected)

    Can.VarTopLevel _ name ->
      return (CLocal region name expected)

    Can.VarKernel _ _ ->
      return CTrue

    Can.VarForeign _ name annotation ->
      return $ CForeign region name annotation expected

    Can.VarCtor _ _ name _ annotation ->
      return $ CForeign region name annotation expected

    Can.VarDebug _ name annotation ->
      return $ CForeign region name annotation expected

    Can.VarOperator op _ _ annotation ->
      return $ CForeign region op annotation expected

    Can.Str _ ->
      return $ CEqual region String Type.string expected

    Can.Chr _ ->
      return $ CEqual region Char Type.char expected

    Can.Int _ ->
      do  var <- mkFlexNumber
          return $ exists [var] $ CEqual region E.Number (VarN var) expected

    Can.Float _ ->
      return $ CEqual region Float Type.float expected

    Can.List elements ->
      constrainList rtv region elements expected

    Can.Negate expr ->
      do  numberVar <- mkFlexNumber
          let numberType = VarN numberVar
          numberCon <- constrain rtv expr (FromContext region Negate numberType)
          let negateCon = CEqual region E.Number numberType expected
          return $ exists [numberVar] $ CAnd [ numberCon, negateCon ]

    Can.Binop op _ _ annotation leftExpr rightExpr ->
      constrainBinop rtv region op annotation leftExpr rightExpr expected

    Can.Lambda args body ->
      constrainLambda rtv region args body expected

    Can.Call func args ->
      constrainCall rtv region func args expected

    Can.If branches finally ->
      constrainIf rtv region branches finally expected

    Can.Case expr branches ->
      constrainCase rtv region expr branches expected

    Can.Let def body ->
      constrainDef rtv def
      =<< constrain rtv body expected

    Can.LetRec defs body ->
      constrainRecursiveDefs rtv defs
      =<< constrain rtv body expected

    Can.LetDestruct pattern expr body ->
      constrainDestruct rtv region pattern expr
      =<< constrain rtv body expected

    Can.Accessor field ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType
          return $ exists [ fieldVar, extVar ] $
            CEqual region (Accessor field) (FunN recordType fieldType) expected

    Can.Access expr (A.At accessRegion field) ->
      do  extVar <- mkFlexVar
          fieldVar <- mkFlexVar
          let extType = VarN extVar
          let fieldType = VarN fieldVar
          let recordType = RecordN (Map.singleton field fieldType) extType

          let context = RecordAccess (A.toRegion expr) (getAccessName expr) accessRegion field
          recordCon <- constrain rtv expr (FromContext region context recordType)

          return $ exists [ fieldVar, extVar ] $
            CAnd
              [ recordCon
              , CEqual region (Access field) fieldType expected
              ]

    Can.Update name expr fields ->
      constrainUpdate rtv region name expr fields expected

    Can.Record fields ->
      constrainRecord rtv region fields expected

    Can.Unit ->
      return $ CEqual region Unit UnitN expected

    Can.Tuple a b maybeC ->
      constrainTuple rtv region a b maybeC expected

    Can.Shader _uid _src glType ->
      constrainShader region glType expected



-- CONSTRAIN LAMBDA


constrainLambda :: RTV -> R.Region -> [Can.Pattern] -> Can.Expr -> Expected Type -> IO Constraint
constrainLambda rtv region args body expected =
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
          , CEqual region Lambda tipe expected
          ]



-- CONSTRAIN CALL


constrainCall :: RTV -> R.Region -> Can.Expr -> [Can.Expr] -> Expected Type -> IO Constraint
constrainCall rtv region func@(A.At funcRegion _) args expected =
  do  let maybeName = getName func

      funcVar <- mkFlexVar
      resultVar <- mkFlexVar
      let funcType = VarN funcVar
      let resultType = VarN resultVar

      funcCon <- constrain rtv func (NoExpectation funcType)

      (argVars, argTypes, argCons) <-
        unzip3 <$> Index.indexedTraverse (constrainArg rtv region maybeName) args

      let arityType = foldr FunN resultType argTypes
      let category = CallResult maybeName

      return $ exists (funcVar:resultVar:argVars) $
        CAnd
          [ funcCon
          , CEqual funcRegion category funcType (FromContext region (CallArity maybeName (length args)) arityType)
          , CAnd argCons
          , CEqual region category resultType expected
          ]


constrainArg :: RTV -> R.Region -> MaybeName -> Index.ZeroBased -> Can.Expr -> IO (Variable, Type, Constraint)
constrainArg rtv region maybeName index arg =
  do  argVar <- mkFlexVar
      let argType = VarN argVar
      argCon <- constrain rtv arg (FromContext region (CallArg maybeName index) argType)
      return (argVar, argType, argCon)


getName :: Can.Expr -> MaybeName
getName (A.At _ expr) =
  case expr of
    Can.VarLocal name        -> FuncName name
    Can.VarTopLevel _ name   -> FuncName name
    Can.VarForeign _ name _  -> FuncName name
    Can.VarCtor _ _ name _ _ -> CtorName name
    Can.VarOperator op _ _ _ -> OpName op
    Can.VarKernel _ name     -> FuncName name
    _                        -> NoName


getAccessName :: Can.Expr -> Maybe N.Name
getAccessName (A.At _ expr) =
  case expr of
    Can.VarLocal name       -> Just name
    Can.VarTopLevel _ name  -> Just name
    Can.VarForeign _ name _ -> Just name
    _                       -> Nothing



-- CONSTRAIN BINOP


constrainBinop :: RTV -> R.Region -> N.Name -> Can.Annotation -> Can.Expr -> Can.Expr -> Expected Type -> IO Constraint
constrainBinop rtv region op annotation leftExpr rightExpr expected =
  do  leftVar <- mkFlexVar
      rightVar <- mkFlexVar
      answerVar <- mkFlexVar
      let leftType = VarN leftVar
      let rightType = VarN rightVar
      let answerType = VarN answerVar
      let binopType = leftType ==> rightType ==> answerType

      let opCon = CForeign region op annotation (NoExpectation binopType)

      leftCon <- constrain rtv leftExpr (FromContext region (OpLeft op) leftType)
      rightCon <- constrain rtv rightExpr (FromContext region (OpRight op) rightType)

      return $ exists [ leftVar, rightVar, answerVar ] $
        CAnd
          [ opCon
          , leftCon
          , rightCon
          , CEqual region (CallResult (OpName op)) answerType expected
          ]



-- CONSTRAIN LISTS


constrainList :: RTV -> R.Region -> [Can.Expr] -> Expected Type -> IO Constraint
constrainList rtv region entries expected =
  do  entryVar <- mkFlexVar
      let entryType = VarN entryVar
      let listType = AppN ModuleName.list N.list [entryType]

      entryCons <-
        Index.indexedTraverse (constrainListEntry rtv region entryType) entries

      return $ exists [entryVar] $
        CAnd
          [ CAnd entryCons
          , CEqual region List listType expected
          ]


constrainListEntry :: RTV -> R.Region -> Type -> Index.ZeroBased -> Can.Expr -> IO Constraint
constrainListEntry rtv region tipe index expr =
  constrain rtv expr (FromContext region (ListEntry index) tipe)



-- CONSTRAIN IF EXPRESSIONS


constrainIf :: RTV -> R.Region -> [(Can.Expr, Can.Expr)] -> Can.Expr -> Expected Type -> IO Constraint
constrainIf rtv region branches final expected =
  do  let boolExpect = FromContext region IfCondition Type.bool
      let (conditions, exprs) = foldr (\(c,e) (cs,es) -> (c:cs,e:es)) ([],[final]) branches

      condCons <-
        traverse (\c -> constrain rtv c boolExpect) conditions

      case expected of
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
                  , CEqual region If branchType expected
                  ]



-- CONSTRAIN CASE EXPRESSIONS


constrainCase :: RTV -> R.Region -> Can.Expr -> [Can.CaseBranch] -> Expected Type -> IO Constraint
constrainCase rtv region expr branches expected =
  do  ptrnVar <- mkFlexVar
      let ptrnType = VarN ptrnVar
      exprCon <- constrain rtv expr (NoExpectation ptrnType)

      case expected of
        FromAnnotation name arity _ tipe ->
          do  branchCons <- Index.indexedForA branches $ \index branch ->
                constrainCaseBranch rtv branch
                  (PFromContext region (PCaseMatch index) ptrnType)
                  (FromAnnotation name arity (TypedCaseBranch index) tipe)

              return $ exists [ptrnVar] $ CAnd (exprCon:branchCons)

        _ ->
          do  branchVar <- mkFlexVar
              let branchType = VarN branchVar

              branchCons <- Index.indexedForA branches $ \index branch ->
                constrainCaseBranch rtv branch
                  (PFromContext region (PCaseMatch index) ptrnType)
                  (FromContext region (CaseBranch index) branchType)

              return $ exists [ptrnVar,branchVar] $
                CAnd
                  [ exprCon
                  , CAnd branchCons
                  , CEqual region Case branchType expected
                  ]


constrainCaseBranch :: RTV -> Can.CaseBranch -> PExpected Type -> Expected Type -> IO Constraint
constrainCaseBranch rtv (Can.CaseBranch pattern expr) pExpect bExpect =
  do  (Pattern.State headers pvars revCons) <-
        Pattern.add pattern pExpect Pattern.emptyState

      CLet [] pvars headers (CAnd (reverse revCons))
        <$> constrain rtv expr bExpect



-- CONSTRAIN RECORD


constrainRecord :: RTV -> R.Region -> Map.Map N.Name Can.Expr -> Expected Type -> IO Constraint
constrainRecord rtv region fields expected =
  do  dict <- traverse (constrainField rtv) fields

      let getType (_, t, _) = t
      let recordType = RecordN (Map.map getType dict) EmptyRecordN
      let recordCon = CEqual region Record recordType expected

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


constrainUpdate :: RTV -> R.Region -> N.Name -> Can.Expr -> Map.Map N.Name Can.FieldUpdate -> Expected Type -> IO Constraint
constrainUpdate rtv region name expr fields expected =
  do  extVar <- mkFlexVar
      fieldDict <- Map.traverseWithKey (constrainUpdateField rtv region) fields

      recordVar <- mkFlexVar
      let recordType = VarN recordVar
      let fieldsType = RecordN (Map.map (\(_,t,_) -> t) fieldDict) (VarN extVar)

      -- NOTE: fieldsType is separate so that Error propagates better
      let fieldsCon = CEqual region Record recordType (NoExpectation fieldsType)
      let recordCon = CEqual region Record recordType expected

      let vars = Map.foldr (\(v,_,_) vs -> v:vs) [recordVar,extVar] fieldDict
      let cons = Map.foldr (\(_,_,c) cs -> c:cs) [recordCon] fieldDict

      con <- constrain rtv expr (FromContext region (RecordUpdateKeys name fields) recordType)

      return $ exists vars $ CAnd (fieldsCon:con:cons)


constrainUpdateField :: RTV -> R.Region -> N.Name -> Can.FieldUpdate -> IO (Variable, Type, Constraint)
constrainUpdateField rtv region field (Can.FieldUpdate _ expr) =
  do  var <- mkFlexVar
      let tipe = VarN var
      con <- constrain rtv expr (FromContext region (RecordUpdateValue field) tipe)
      return (var, tipe, con)



-- CONSTRAIN TUPLE


constrainTuple :: RTV -> R.Region -> Can.Expr -> Can.Expr -> Maybe Can.Expr -> Expected Type -> IO Constraint
constrainTuple rtv region a b maybeC expected =
  do  aVar <- mkFlexVar
      bVar <- mkFlexVar
      let aType = VarN aVar
      let bType = VarN bVar

      aCon <- constrain rtv a (NoExpectation aType)
      bCon <- constrain rtv b (NoExpectation bType)

      case maybeC of
        Nothing ->
          do  let tupleType = TupleN aType bType Nothing
              let tupleCon = CEqual region Tuple tupleType expected
              return $ exists [ aVar, bVar ] $ CAnd [ aCon, bCon, tupleCon ]

        Just c ->
          do  cVar <- mkFlexVar
              let cType = VarN cVar

              cCon <- constrain rtv c (NoExpectation cType)

              let tupleType = TupleN aType bType (Just cType)
              let tupleCon = CEqual region Tuple tupleType expected

              return $ exists [ aVar, bVar, cVar ] $ CAnd [ aCon, bCon, cCon, tupleCon ]



-- CONSTRAIN SHADER


constrainShader :: R.Region -> Shader.Shader -> Expected Type -> IO Constraint
constrainShader region (Shader.Shader attributes uniforms varyings) expected =
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
        CEqual region Shader shaderType expected


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
        Pattern.add pattern (PNoExpectation patternType) Pattern.emptyState

      exprCon <-
        constrain rtv expr (FromContext region Destructure patternType)

      return $ CLet [] (patternVar:pvars) headers (CAnd (reverse (exprCon:revCons))) bodyCon



-- CONSTRAIN DEF


constrainDef :: RTV -> Can.Def -> Constraint -> IO Constraint
constrainDef rtv def bodyCon =
  case def of
    Can.Def (A.At region name) args expr ->
      do  (Args vars tipe resultType (Pattern.State headers pvars revCons)) <-
            constrainArgs args

          exprCon <-
            constrain rtv expr (NoExpectation resultType)

          return $
            CLet
              { _rigidVars = []
              , _flexVars = vars
              , _header = Map.singleton name (A.At region tipe)
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

    Can.TypedDef (A.At region name) freeVars typedArgs expr srcResultType ->
      do  let newNames = Map.difference freeVars rtv
          newRigids <- Map.traverseWithKey (\n _ -> nameToRigid n) newNames
          let newRtv = Map.union rtv (Map.map VarN newRigids)

          (TypedArgs tipe resultType (Pattern.State headers pvars revCons)) <-
            constrainTypedArgs newRtv name typedArgs srcResultType

          let expected = FromAnnotation name (length typedArgs) TypedBody resultType
          exprCon <-
            constrain newRtv expr expected

          return $
            CLet
              { _rigidVars = Map.elems newRigids
              , _flexVars = []
              , _header = Map.singleton name (A.At region tipe)
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
        Can.Def (A.At region name) args expr ->
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
                  , _headers = Map.insert name (A.At region tipe) flexHeaders
                  }

        Can.TypedDef (A.At region name) freeVars typedArgs expr srcResultType ->
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
                    , _cons = CLet (Map.elems newRigids) [] Map.empty defCon CTrue : rigidCons
                    , _headers = Map.insert name (A.At region tipe) rigidHeaders
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


constrainArgs :: [Can.Pattern] -> IO Args
constrainArgs args =
  argsHelp args Pattern.emptyState


argsHelp :: [Can.Pattern] -> Pattern.State -> IO Args
argsHelp args state =
  case args of
    [] ->
      do  resultVar <- mkFlexVar
          let resultType = VarN resultVar
          return $ Args [resultVar] resultType resultType state

    pattern : otherArgs ->
      do  argVar <- mkFlexVar
          let argType = VarN argVar

          (Args vars tipe result newState) <-
            argsHelp otherArgs =<<
              Pattern.add pattern (PNoExpectation argType) state

          return (Args (argVar:vars) (FunN argType tipe) result newState)



-- CONSTRAIN TYPED ARGS


data TypedArgs =
  TypedArgs
    { _t_type :: Type
    , _t_result :: Type
    , _t_state :: Pattern.State
    }


constrainTypedArgs :: Map.Map N.Name Type -> N.Name -> [(Can.Pattern, Can.Type)] -> Can.Type -> IO TypedArgs
constrainTypedArgs rtv name args srcResultType =
  typedArgsHelp rtv name Index.first args srcResultType Pattern.emptyState


typedArgsHelp :: Map.Map N.Name Type -> N.Name -> Index.ZeroBased -> [(Can.Pattern, Can.Type)] -> Can.Type -> Pattern.State -> IO TypedArgs
typedArgsHelp rtv name index args srcResultType state =
  case args of
    [] ->
      do  resultType <- Instantiate.fromSrcType rtv srcResultType
          return $ TypedArgs resultType resultType state

    (pattern@(A.At region _), srcType) : otherArgs ->
      do  argType <- Instantiate.fromSrcType rtv srcType
          let expected = PFromContext region (PTypedArg name index) argType

          (TypedArgs tipe resultType newState) <-
            typedArgsHelp rtv name (Index.next index) otherArgs srcResultType =<<
              Pattern.add pattern expected state

          return (TypedArgs (FunN argType tipe) resultType newState)
