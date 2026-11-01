{-# LANGUAGE BangPatterns, MagicHash #-}
module Type.Constrain.Expression
  ( constrainDecl
  )
  where


import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyMap as NEM

import qualified Array as A

import qualified AST.Canonical as Can
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.Shader as Shader
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import Reporting.Error.Type (Expected(..), Context(..), SubContext(..), MaybeName(..), Category(..), PExpected(..), PContext(..))
import qualified Type.Constrain.Pattern as Pattern
import Type.Type as Type hiding (Descriptor(..))



-- CONSTRAIN DECLARATION


constrainDecl :: Can.Decl -> Constraint -> IO Constraint
constrainDecl decl bodyCon =
  case decl of
    Can.Decl    def  -> conDecl def bodyCon
    Can.DeclRec d ds -> conDeclRec d ds bodyCon


conDecl :: Can.Def -> Constraint -> IO Constraint
conDecl def bodyCon =
  case def of
    Can.DValue_ (A.At region name) expr ->
      do  var <- mkFlexAny
          let tipe = VarN var
          exprCon <- constrain rtv expr $ NoExpectation tipe
          return $ CLet [] [var] (Map.singleton name (A.At region tipe)) exprCon bodyCon

    Can.DValue (A.At region name) expr iType (I.Forall freeVars _) ->
      do  (localVars, (newRtv, newRigids)) <- A.foldlMapIO addRTV (rtv, []) freeVars
          let tipe = TypeN localVars iType
          exprCon <- constrain newRtv expr $ FromAnnotation name 0 TypedBody tipe
          return $ CLet newRigids [] (Map.singleton name (A.At region tipe)) exprCon bodyCon

    Can.DFunc_ n p ps e     -> conFunc_ rtv n p ps e     bodyCon
    Can.DFunc  n p ps e t a -> conFunc  rtv n p ps e t a bodyCon
  where
    rtv = RTV Map.empty


conDeclRec :: Can.Def -> [Can.Def] -> Constraint -> IO Constraint
conDeclRec def0 defs0 bodyCon =
    step def0 emptyInfo emptyInfo (loop defs0)
  where
    rtv = RTV Map.empty

    loop defs rigidInfo flexInfo =
      case defs of
        d:ds ->
          step d rigidInfo flexInfo (loop ds)

        [] ->
          do  let (Info rigidVars rigidCons rigidHeaders) = rigidInfo
              let (Info flexVars  flexCons  flexHeaders ) = flexInfo
              return $
                CLet rigidVars [] rigidHeaders CTrue $
                  CLet [] flexVars flexHeaders (CLet [] [] flexHeaders CTrue (CAnd flexCons)) $
                    CAnd [ CAnd rigidCons, bodyCon ]

    step def rigidInfo flexInfo cont =
      case def of
        Can.DValue_ (A.At region name) expr ->
          do  var <- mkFlexAny
              let tipe = VarN var
              exprCon <- constrain rtv expr (NoExpectation tipe)
              cont rigidInfo (addInfo [var] exprCon name region tipe flexInfo)

        Can.DValue (A.At region name) expr iType (I.Forall freeVars _) ->
          do  (localVars, (newRtv, newRigids)) <- A.foldlMapIO addRTV (rtv, []) freeVars
              let tipe = TypeN localVars iType
              exprCon <- constrain newRtv expr $ FromAnnotation name 0 TypedBody tipe
              let rigidCon = CLet newRigids [] Map.empty exprCon CTrue
              cont (addInfo newRigids rigidCon name region tipe rigidInfo) flexInfo

        Can.DFunc_ (A.At region name) arg args expr ->
          do  (Args newFlexVars tipe resultType state) <- conArgs arg args
              let (Pattern.State headers pvars revCons) = state
              exprCon <- constrain rtv expr (NoExpectation resultType)
              let letCon = CLet [] pvars headers (CAnd (List.reverse revCons)) exprCon
              cont rigidInfo (addInfo newFlexVars letCon name region tipe flexInfo)

        Can.DFunc (A.At region name) arg args expr iResultType (I.Forall freeVars _) ->
          do  (localVars, (newRtv, newRigids)) <- A.foldlMapIO addRTV (rtv, []) freeVars
              let resultType = TypeN localVars iResultType
              (TypedArgs tipe state) <- conTypedArgs localVars name arg args resultType
              let (Pattern.State headers pvars revCons) = state
              let expected = FromAnnotation name (1 + List.length args) TypedBody resultType
              exprCon <- constrain newRtv expr expected
              let letCon = CLet [] pvars headers (CAnd (List.reverse revCons)) exprCon
              let rigidCon = CLet newRigids [] Map.empty letCon CTrue
              cont (addInfo newRigids rigidCon name region tipe rigidInfo) flexInfo



-- CONSTRAIN


-- As we step past type annotations, the free type variables are added to
-- the "rigid type variables" dict. Allowing sharing of rigid variables
-- between nested type annotations.
--
-- So if you have a top-level type annotation like (func : a -> b) the RTV
-- dictionary will hold variables for `a` and `b`
--
newtype RTV =
  RTV (Map.Map T.Var Variable)


constrain :: RTV -> Can.Expr -> Expected Type -> IO Constraint
constrain rtv (A.At r expression) expected =
  case expression of
    Can.VarLocal n           -> return $ CLocal    r n    expected
    Can.VarTopLevel _ n      -> return $ CLocal    r n    expected
    Can.VarForeign _ n t     -> return $ CForeign  r n  t expected
    Can.VarDebug _ n t       -> return $ CForeign  r n  t expected
    Can.VarOperator op _ _ t -> return $ COperator r op t expected
    Can.VarKernel _ _        -> return CTrue
    Can.CRecord  _ n t _ _   -> return $ CForeign r n t expected
    Can.CVariant _ n t _ _   -> return $ CForeign r n t expected
    Can.CEnum    _ n t _     -> return $ CForeign r n t expected
    Can.CVector  _ n t _     -> return $ CForeign r n t expected
    Can.CBox     _ n t       -> return $ CForeign r n t expected
    Can.CVoid    _ n t       -> return $ CForeign r n t expected
    Can.Empty                -> return $ CEqual r Record EmptyN      expected
    Can.Unit                 -> return $ CEqual r Unit   UnitN       expected
    Can.Bool  _              -> return $ CEqual r Bool   Type.bool   expected
    Can.Str   _              -> return $ CEqual r String Type.string expected
    Can.Chr   _              -> return $ CEqual r Char   Type.char   expected
    Can.Int   _              -> do { v <- mkFlexNumber ; return $ exists [v] $ CEqual r E.Number (VarN v) expected }
    Can.Float _              -> return $ CEqual r Float Type.float expected
    Can.List es              -> conList     rtv r es       expected
    Can.Negate e             -> conNegate   rtv r e        expected
    Can.Binop op _ _ t a b   -> conBinop    rtv r op t a b expected
    Can.Lambda x xs b        -> conLambda   rtv r x xs b   expected
    Can.Call f x xs          -> conCall     rtv r f x xs   expected
    Can.If thens e           -> conIf       rtv r thens e  expected
    Can.Case e b bs          -> conCase     rtv r e (b:bs) expected
    Can.LetValue v e         -> conLetValue rtv v e        expected
    Can.LetFunc f e          -> conLetFunc  rtv f     =<< constrain rtv e expected
    Can.LetRec f fs e        -> conLetRec   rtv f fs  =<< constrain rtv e expected
    Can.LetDestruct p e b    -> conDestruct rtv r p e b    expected
    Can.Pair   a b           -> conPair     rtv r a b      expected
    Can.Triple a b c         -> conTriple   rtv r a b c    expected
    Can.Shader _ ts          -> conShader       r ts       expected
    Can.Update n e fs        -> conUpdate   rtv r n e fs   expected
    Can.Record fs            -> conRecord   rtv r fs       expected
    Can.Accessor f           -> conAccessor rtv r f        expected
    Can.Access e f           -> conAccess   rtv r e f      expected



-- CONSTRAIN NEGATE


conNegate :: RTV -> A.Region -> Can.Expr -> Expected Type -> IO Constraint
conNegate rtv region expr expected =
  do  numberVar <- mkFlexNumber
      let numberType = VarN numberVar
      numberCon <- constrain rtv expr (FromContext region Negate numberType)
      let negateCon = CEqual region E.Number numberType expected
      return $ exists [numberVar] $ CAnd [ numberCon, negateCon ]



-- CONSTRAIN LAMBDA


conLambda :: RTV -> A.Region -> Can.Pattern -> [Can.Pattern] -> Can.Expr -> Expected Type -> IO Constraint
conLambda rtv region arg args body expected =
  do  (Args vars tipe resultType (Pattern.State headers pvars revCons)) <-
        conArgs arg args

      bodyCon <-
        constrain rtv body (NoExpectation resultType)

      return $ exists vars $
        CAnd
          [ CLet [] pvars headers (CAnd (List.reverse revCons)) bodyCon
          , CEqual region Lambda tipe expected
          ]



-- CONSTRAIN CALL


conCall :: RTV -> A.Region -> Can.Expr -> Can.Expr -> [Can.Expr] -> Expected Type -> IO Constraint
conCall rtv region func@(A.At funcRegion _) arg args expected =
  do  let maybeName = getName func

      funcVar <- mkFlexAny
      resultVar <- mkFlexAny
      let funcType = VarN funcVar
      let resultType = VarN resultVar

      funcCon <- constrain rtv func (NoExpectation funcType)

      (argVars, argTypes, argCons) <-
        unzip3 <$> Index.indexedTraverse (conArg rtv region maybeName) (arg:args)

      let arityType = foldr FunN resultType argTypes
      let category = CallResult maybeName

      return $ exists (funcVar:resultVar:argVars) $
        CAnd
          [ funcCon
          , CEqual funcRegion category funcType (FromContext region (CallArity maybeName (length args)) arityType)
          , CAnd argCons
          , CEqual region category resultType expected
          ]


conArg :: RTV -> A.Region -> MaybeName -> Index.ZeroBased -> Can.Expr -> IO (Variable, Type, Constraint)
conArg rtv region maybeName index arg =
  do  argVar <- mkFlexAny
      let argType = VarN argVar
      argCon <- constrain rtv arg (FromContext region (CallArg maybeName index) argType)
      return (argVar, argType, argCon)


getName :: Can.Expr -> MaybeName
getName (A.At _ expr) =
  case expr of
    Can.VarLocal n          -> FuncName n
    Can.VarTopLevel _ n     -> FuncName n
    Can.VarForeign _ n _    -> FuncName n
    Can.VarDebug _ n _      -> FuncName n
    Can.VarOperator n _ _ _ -> OpName n
    Can.VarKernel _ n       -> FuncName n
    Can.CRecord  _ n _ _ _  -> RcrdName n
    Can.CVariant _ n _ _ _  -> CtorName n
    Can.CEnum    _ n _ _    -> CtorName n
    Can.CVector  _ n _ _    -> CtorName n
    Can.CBox     _ n _      -> CtorName n
    Can.CVoid    _ n _      -> CtorName n
    _                       -> NoName


getAccessName :: Can.Expr -> Maybe N.Name
getAccessName (A.At _ expr) =
  case expr of
    Can.VarLocal n       -> Just n
    Can.VarTopLevel _ n  -> Just n
    Can.VarForeign _ n _ -> Just n
    _                    -> Nothing



-- CONSTRAIN BINOP


conBinop :: RTV -> A.Region -> Op.Name -> I.Annotation -> Can.Expr -> Can.Expr -> Expected Type -> IO Constraint
conBinop rtv region op annotation leftExpr rightExpr expected =
  do  leftVar <- mkFlexAny
      rightVar <- mkFlexAny
      answerVar <- mkFlexAny
      let leftType = VarN leftVar
      let rightType = VarN rightVar
      let answerType = VarN answerVar
      let binopType = leftType ==> rightType ==> answerType

      let opCon = COperator region op annotation (NoExpectation binopType)

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


conList :: RTV -> A.Region -> [Can.Expr] -> Expected Type -> IO Constraint
conList rtv region entries expected =
  do  entryVar <- mkFlexAny
      let entryType = VarN entryVar
      let listType = AppN ModuleName.list T.list M.list (A.from1 entryType)

      entryCons <-
        Index.indexedTraverse (conListEntry rtv region entryType) entries

      return $ exists [entryVar] $
        CAnd
          [ CAnd entryCons
          , CEqual region List listType expected
          ]


conListEntry :: RTV -> A.Region -> Type -> Index.ZeroBased -> Can.Expr -> IO Constraint
conListEntry rtv region tipe index expr =
  constrain rtv expr (FromContext region (ListEntry index) tipe)



-- CONSTRAIN IF EXPRESSIONS


conIf :: RTV -> A.Region -> [(Can.Expr, Can.Expr)] -> Can.Expr -> Expected Type -> IO Constraint
conIf rtv region branches final expected =
  case expected of
    NoExpectation        _ -> without
    FromContext    _   _ _ -> without
    FromAnnotation n a _ t -> with n a t
  where
    with name arity tipe =
      do  let boolExpect = FromContext region IfCondition Type.bool
          let (conditions, exprs) = foldr (\(c,e) (cs,es) -> (c:cs,e:es)) ([],[final]) branches

          condCons <-
            traverse (\c -> constrain rtv c boolExpect) conditions

          branchCons <- Index.indexedForA exprs $ \index expr ->
            constrain rtv expr (FromAnnotation name arity (TypedIfBranch index) tipe)

          return $
            CAnd
              [ CAnd condCons
              , CAnd branchCons
              ]

    without =
      do  let boolExpect = FromContext region IfCondition Type.bool
          let (conditions, exprs) = foldr (\(c,e) (cs,es) -> (c:cs,e:es)) ([],[final]) branches

          condCons <-
            traverse (\c -> constrain rtv c boolExpect) conditions

          branchVar <- mkFlexAny
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


conCase :: RTV -> A.Region -> Can.Expr -> [Can.Branch] -> Expected Type -> IO Constraint
conCase rtv region expr branches expected =
  case expected of
    NoExpectation        _ -> without
    FromContext      _ _ _ -> without
    FromAnnotation n a _ t -> with n a t
  where
    with name arity tipe =
      do  ptrnVar <- mkFlexAny
          let ptrnType = VarN ptrnVar
          exprCon <- constrain rtv expr (NoExpectation ptrnType)

          branchCons <- Index.indexedForA branches $ \index branch ->
            conCaseBranch rtv branch
              (PFromContext region (PCaseMatch index) ptrnType)
              (FromAnnotation name arity (TypedCaseBranch index) tipe)

          return $ exists [ptrnVar] $ CAnd (exprCon:branchCons)

    without =
      do  ptrnVar <- mkFlexAny
          let ptrnType = VarN ptrnVar
          exprCon <- constrain rtv expr (NoExpectation ptrnType)

          branchVar <- mkFlexAny
          let branchType = VarN branchVar

          branchCons <- Index.indexedForA branches $ \index branch ->
            conCaseBranch rtv branch
              (PFromContext region (PCaseMatch index) ptrnType)
              (FromContext region (CaseBranch index) branchType)

          return $ exists [ptrnVar,branchVar] $
            CAnd
              [ exprCon
              , CAnd branchCons
              , CEqual region Case branchType expected
              ]


conCaseBranch :: RTV -> Can.Branch -> PExpected Type -> Expected Type -> IO Constraint
conCaseBranch rtv (Can.Branch pattern expr) pExpect bExpect =
  do  (Pattern.State headers pvars revCons) <-
        Pattern.add pattern pExpect Pattern.emptyState

      CLet [] pvars headers (CAnd (reverse revCons))
        <$> constrain rtv expr bExpect



-- CONSTRAIN RECORD


conRecord :: RTV -> A.Region -> F.Fields Can.Expr -> Expected Type -> IO Constraint
conRecord rtv region (F.Fields order fields) expected =
  do  dict <- NEM.traverse (conField rtv) fields

      let recordType = RecordN order (NEM.map (\(_,t,_) -> t) dict)
      let recordCon  = CEqual region Record recordType expected

      let vars = NEM.foldr (\(v,_,_) vs -> v:vs) [] dict
      let cons = NEM.foldr (\(_,_,c) cs -> c:cs) [recordCon] dict

      return $ exists vars (CAnd cons)


conField :: RTV -> Can.Expr -> IO (Variable, Type, Constraint)
conField rtv expr =
  do  var <- mkFlexAny
      let tipe = VarN var
      con <- constrain rtv expr (NoExpectation tipe)
      return (var, tipe, con)



-- CONSTRAIN RECORD UPDATE


conUpdate :: RTV -> A.Region -> N.Name -> Can.Expr -> F.Fields Can.Update -> Expected Type -> IO Constraint
conUpdate rtv region name expr (F.Fields order dict) expected =
  do  extVar    <- mkFlexAny
      recordVar <- mkFlexAny
      fieldDict <- NEM.traverseWithKey (conUpdateField rtv region) dict
      let recordType = VarN recordVar
      let fieldsType = ExtRecN order (NEM.map (\(_,t,_) -> t) fieldDict) (VarN extVar)

      -- NOTE: fieldsType is separate so that Error propagates better
      let fieldsCon = CEqual region Record recordType (NoExpectation fieldsType)
      let recordCon = CEqual region Record recordType expected

      let vars = NEM.foldr (\(v,_,_) vs -> v:vs) [recordVar,extVar] fieldDict
      let cons = NEM.foldr (\(_,_,c) cs -> c:cs) [recordCon] fieldDict

      con <- constrain rtv expr (FromContext region (RecordUpdateKeys name dict) recordType)

      return $ exists vars $ CAnd (fieldsCon:con:cons)


conUpdateField :: RTV -> A.Region -> N.Name -> Can.Update -> IO (Variable, Type, Constraint)
conUpdateField rtv region field (Can.Update_ _ expr) =
  do  var <- mkFlexAny
      let tipe = VarN var
      con <- constrain rtv expr (FromContext region (RecordUpdateValue field) tipe)
      return (var, tipe, con)



-- CONSTRAIN ACCESSOR


conAccessor :: RTV -> A.Region -> N.Name -> Expected Type -> IO Constraint
conAccessor _ region field expected =
  do  extVar   <- mkFlexAny
      fieldVar <- mkFlexAny
      let extType    = VarN extVar
      let fieldType  = VarN fieldVar
      let recordType = ExtRecN order (NEM.singleton field fieldType) extType
      return $ exists [ fieldVar, extVar ] $
        CEqual region (Accessor field) (FunN recordType fieldType) expected
  where
    !(F.Order order) = F.unordered



-- CONSTRAIN ACCESS


conAccess :: RTV -> A.Region -> Can.Expr -> A.Located N.Name -> Expected Type -> IO Constraint
conAccess rtv region expr (A.At accessRegion field) expected =
  do  extVar   <- mkFlexAny
      fieldVar <- mkFlexAny
      let extType    = VarN extVar
      let fieldType  = VarN fieldVar
      let recordType = ExtRecN order (NEM.singleton field fieldType) extType
      let context    = RecordAccess (A.toRegion expr) (getAccessName expr) accessRegion field
      recordCon <- constrain rtv expr (FromContext region context recordType)
      return $ exists [ fieldVar, extVar ] $
        CAnd
          [ recordCon
          , CEqual region (Access field) fieldType expected
          ]
  where
    !(F.Order order) = F.unordered



-- CONSTRAIN TUPLES


conPair :: RTV -> A.Region -> Can.Expr -> Can.Expr -> Expected Type -> IO Constraint
conPair rtv region a b expected =
  do  aVar <- mkFlexAny
      bVar <- mkFlexAny
      let aType = VarN aVar
      let bType = VarN bVar

      aCon <- constrain rtv a (NoExpectation aType)
      bCon <- constrain rtv b (NoExpectation bType)

      let tupleType = PairN aType bType
      let tupleCon = CEqual region Tuple tupleType expected

      return $ exists [ aVar, bVar ] $ CAnd [ aCon, bCon, tupleCon ]


conTriple :: RTV -> A.Region -> Can.Expr -> Can.Expr -> Can.Expr -> Expected Type -> IO Constraint
conTriple rtv region a b c expected =
  do  aVar <- mkFlexAny
      bVar <- mkFlexAny
      cVar <- mkFlexAny
      let aType = VarN aVar
      let bType = VarN bVar
      let cType = VarN cVar

      aCon <- constrain rtv a (NoExpectation aType)
      bCon <- constrain rtv b (NoExpectation bType)
      cCon <- constrain rtv c (NoExpectation cType)

      let tupleType = TripleN aType bType cType
      let tupleCon = CEqual region Tuple tupleType expected

      return $ exists [ aVar, bVar, cVar ] $ CAnd [ aCon, bCon, cCon, tupleCon ]



-- CONSTRAIN SHADER


conShader :: A.Region -> Shader.Types -> Expected Type -> IO Constraint
conShader region (Shader.Types attributes uniforms varyings) expected =
  do  attrVar <- mkFlexAny
      unifVar <- mkFlexAny

      let shaderType =
            AppN ModuleName.webgl T.shader M.shader $
              A.from3 (toOpen attributes attrVar) (toOpen uniforms unifVar) (toClosed varyings)

      return $ exists [ attrVar, unifVar ] $
        CEqual region Shader shaderType expected
  where
    toOpen types ext =
      case NEM.fromDict types of
        Nothing  -> VarN ext
        Just nem -> ExtRecN order (NEM.map glToType nem) (VarN ext)

    toClosed types =
      case NEM.fromDict types of
        Nothing  -> EmptyN
        Just nem -> RecordN order (NEM.map glToType nem)

    !(F.Order order) = F.unordered


glToType :: Shader.Type -> Type
glToType glType =
  case glType of
    Shader.V2      -> Type.vec2
    Shader.V3      -> Type.vec3
    Shader.V4      -> Type.vec4
    Shader.M4      -> Type.mat4
    Shader.Int     -> Type.int
    Shader.Float   -> Type.float
    Shader.Texture -> Type.texture



-- CONSTRAIN DESTRUCTURES


conDestruct :: RTV -> A.Region -> Can.Pattern -> Can.Expr -> Can.Expr -> Expected Type -> IO Constraint
conDestruct rtv region pattern expr body expected =
  do  var <- mkFlexAny
      let tipe = VarN var
      (Pattern.State headers pvars revCons) <- Pattern.add pattern (PNoExpectation tipe) Pattern.emptyState
      exprCon <- constrain rtv expr (FromContext region Destructure tipe)
      bodyCon <- constrain rtv body expected
      return $ CLet [] (var:pvars) headers (CAnd (List.reverse (exprCon:revCons))) bodyCon



-- CONSTRAIN LET VALUE


conLetValue :: RTV -> Can.Value -> Can.Expr -> Expected Type -> IO Constraint
conLetValue rtv value body expected =
  case value of
    Can.Value_ (A.At region name) expr ->
      do  var <- mkFlexAny
          let tipe = VarN var
          exprCon <- constrain rtv expr (NoExpectation tipe)
          bodyCon <- constrain rtv body expected
          return $ CLet [] [var] (Map.singleton name (A.At region tipe)) exprCon bodyCon

    Can.Value (A.At region name) expr iType (I.Forall freeVars _) ->
      do  (localVars, (newRtv, newRigids)) <- A.foldlMapIO addRTV (rtv, []) freeVars
          let tipe = TypeN localVars iType
          exprCon <- constrain newRtv expr (FromAnnotation name 0 TypedBody tipe)
          bodyCon <- constrain rtv body expected
          return $ CLet newRigids [] (Map.singleton name (A.At region tipe)) exprCon bodyCon



-- CONSTRAIN LET FUNC
--
-- The helpers conFunc/conFunc_ are shared with constrainDecl.


conLetFunc :: RTV -> Can.Func -> Constraint -> IO Constraint
conLetFunc rtv func bodyCon =
  case func of
    Can.Func_ n p ps e     -> conFunc_ rtv n p ps e     bodyCon
    Can.Func  n p ps e t a -> conFunc  rtv n p ps e t a bodyCon


conFunc_ :: RTV -> A.Located N.Name -> Can.Pattern -> [Can.Pattern] -> Can.Expr -> Constraint -> IO Constraint
conFunc_ rtv (A.At region name) arg args expr bodyCon =
  do  (Args vars tipe resultType (Pattern.State headers pvars revCons)) <- conArgs arg args
      exprCon <- constrain rtv expr (NoExpectation resultType)
      return $
        CLet
          { _rigidVars = []
          , _flexVars  = vars
          , _header    = Map.singleton name (A.At region tipe)
          , _headerCon = CLet [] pvars headers (CAnd (List.reverse revCons)) exprCon
          , _bodyCon   = bodyCon
          }


conFunc :: RTV -> A.Located N.Name -> Can.PArg -> [Can.PArg] -> Can.Expr -> I.Type -> I.Annotation -> Constraint -> IO Constraint
conFunc rtv (A.At region name) arg args expr iResultType (I.Forall freeVars _) bodyCon =
  do  (localVars, (newRtv, newRigids)) <- A.foldlMapIO addRTV (rtv, []) freeVars
      let resultType = TypeN localVars iResultType
      (TypedArgs tipe (Pattern.State headers pvars revCons)) <- conTypedArgs localVars name arg args resultType
      exprCon <- constrain newRtv expr $ FromAnnotation name (1 + List.length args) TypedBody resultType
      return $
        CLet
          { _rigidVars = newRigids
          , _flexVars  = []
          , _header    = Map.singleton name (A.At region tipe)
          , _headerCon = CLet [] pvars headers (CAnd (List.reverse revCons)) exprCon
          , _bodyCon   = bodyCon
          }



-- CONSTRAIN LET REC


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


addInfo :: [Variable] -> Constraint -> N.Name -> A.Region -> Type -> Info -> Info
addInfo newVars con name region tipe (Info vars cons headers) =
  Info (newVars ++ vars) (con : cons) (Map.insert name (A.At region tipe) headers)


conLetRec :: RTV -> Can.Func -> [Can.Func] -> Constraint -> IO Constraint
conLetRec rtv func0 funcs0 bodyCon =
    step func0 emptyInfo emptyInfo (loop funcs0)
  where
    loop funcs rigidInfo flexInfo =
      case funcs of
        f:fs ->
          step f rigidInfo flexInfo (loop fs)

        [] ->
          do  let (Info rigidVars rigidCons rigidHeaders) = rigidInfo
              let (Info flexVars  flexCons  flexHeaders ) = flexInfo
              return $
                CLet rigidVars [] rigidHeaders CTrue $
                  CLet [] flexVars flexHeaders (CLet [] [] flexHeaders CTrue (CAnd flexCons)) $
                    CAnd [ CAnd rigidCons, bodyCon ]

    step func rigidInfo flexInfo cont =
      case func of
        Can.Func_ (A.At region name) arg args expr ->
          do  (Args newFlexVars tipe resultType state) <- conArgs arg args
              let (Pattern.State headers pvars revCons) = state
              exprCon <- constrain rtv expr (NoExpectation resultType)
              let letCon = CLet [] pvars headers (CAnd (List.reverse revCons)) exprCon
              cont rigidInfo (addInfo newFlexVars letCon name region tipe flexInfo)

        Can.Func (A.At region name) arg args expr iResultType (I.Forall freeVars _) ->
          do  (localVars, (newRtv, newRigids)) <- A.foldlMapIO addRTV (rtv, []) freeVars
              let resultType = TypeN localVars iResultType
              (TypedArgs tipe state) <- conTypedArgs localVars name arg args resultType
              let (Pattern.State headers pvars revCons) = state
              exprCon <- constrain newRtv expr $ FromAnnotation name (1 + List.length args) TypedBody resultType
              let letCon = CLet [] pvars headers (CAnd (List.reverse revCons)) exprCon
              let rigidCon = CLet newRigids [] Map.empty letCon CTrue
              cont (addInfo newRigids rigidCon name region tipe rigidInfo) flexInfo



-- ADD RIGID


addRTV :: T.Var -> (RTV, [Variable]) -> IO (Variable, (RTV, [Variable]))
addRTV var (RTV dict, newRigids) =
  case Map.lookup var dict of
    Nothing ->
      do  v <- varToRigid var
          let newDict = Map.insert var v dict
          return (v, (RTV newDict, v:newRigids))

    Just v ->
      return (v, (RTV dict, newRigids))



-- CONSTRAIN ARGS


data Args =
  Args
    { _a_vars :: [Variable]
    , _a_type :: Type
    , _a_result :: Type
    , _a_state :: Pattern.State
    }


conArgs :: Can.Pattern -> [Can.Pattern] -> IO Args
conArgs arg args =
    add1 arg args Pattern.emptyState
  where
    add1 p ps state =
      do  argVar <- mkFlexAny
          let argType = VarN argVar

          (Args vars tipe result newState) <-
            addN ps =<< Pattern.add p (PNoExpectation argType) state

          return (Args (argVar:vars) (FunN argType tipe) result newState)

    addN patterns state =
      case patterns of
        [] ->
          do  resultVar <- mkFlexAny
              let resultType = VarN resultVar
              return $ Args [resultVar] resultType resultType state

        p:ps ->
          add1 p ps state



-- CONSTRAIN TYPED ARGS


data TypedArgs =
  TypedArgs
    { _t_type :: Type
    , _t_state :: Pattern.State
    }


conTypedArgs :: A.Array Variable -> N.Name -> Can.PArg -> [Can.PArg] -> Type -> IO TypedArgs
conTypedArgs freeVars name arg args resultType =
    add1 arg args Pattern.emptyState
  where
    add1 (Can.PArg index tipe pattern@(A.At region _)) otherArgs state =
      do  let argType = TypeN freeVars tipe
          let expected = PFromContext region (PTypedArg name index) argType

          (TypedArgs otherType newState) <- addN otherArgs =<< Pattern.add pattern expected state

          return $ TypedArgs (FunN argType otherType) newState

    addN patterns state =
      case patterns of
        []   -> return $ TypedArgs resultType state
        p:ps -> add1 p ps state

