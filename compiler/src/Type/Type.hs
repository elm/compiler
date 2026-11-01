{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, Rank2Types,
TemplateHaskell, UnboxedTuples
#-}
module Type.Type
  ( Constraint(..)
  , exists
  , Type(..)
  , (==>)
  , int, float, char, string, bool, never
  , vec2, vec3, vec4, mat4, texture
  --
  , module Type.Variable
  --
  , toAnnotation
  , toErrorType
  )
  where


import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyMap as NEM
import GHC.Exts (isTrue#)
import GHC.Int (Int(..))
import GHC.IO (IO(IO))
import GHC.Prim

import qualified Array as A
import qualified Crash
import qualified String as S

import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as E
import qualified Reporting.Render.Type as Err
import qualified Type.UnionFind as UF
import Type.Variable



-- CONSTRAINTS


data Constraint
  = CTrue
  | CSaveTheEnvironment
  | CEqual A.Region E.Category Type (E.Expected Type)
  | CLocal A.Region N.Name (E.Expected Type)
  | CForeign A.Region N.Name I.Annotation (E.Expected Type)
  | COperator A.Region Op.Name I.Annotation (E.Expected Type)
  | CPattern A.Region E.PCategory Type (E.PExpected Type)
  | CAnd [Constraint]
  | CLet
      { _rigidVars :: [Variable]
      , _flexVars :: [Variable]
      , _header :: Map.Map N.Name (A.Located Type)
      , _headerCon :: Constraint
      , _bodyCon :: Constraint
      }


exists :: [Variable] -> Constraint -> Constraint
exists flexVars constraint =
  CLet [] flexVars Map.empty constraint CTrue



-- TYPES


data Type
  = VarN Variable
  | AppN ModuleName.Canonical T.Name M.Mask (A.Array Type)
  | FunN Type Type
  | TypeN (A.Array Variable) I.Type
  | EmptyN
  | UnitN
  | PairN Type Type
  | TripleN Type Type Type
  | RecordN F.Order# (NEM.Map N.Name Type)
  | ExtRecN F.Order# (NEM.Map N.Name Type) Type



-- FUNCTION TYPES


infixr 9 ==>


{-# INLINE (==>) #-}
(==>) :: Type -> Type -> Type
(==>) =
  FunN



-- PRIMITIVE TYPES


{-# NOINLINE int    #-}; int    :: Type; int    = AppN ModuleName.basics T.int    M.int    A.empty
{-# NOINLINE float  #-}; float  :: Type; float  = AppN ModuleName.basics T.float  M.float  A.empty
{-# NOINLINE char   #-}; char   :: Type; char   = AppN ModuleName.char   T.char   M.char   A.empty
{-# NOINLINE string #-}; string :: Type; string = AppN ModuleName.string T.string M.string A.empty
{-# NOINLINE bool   #-}; bool   :: Type; bool   = AppN ModuleName.basics T.bool   M.bool   A.empty
{-# NOINLINE never  #-}; never  :: Type; never  = AppN ModuleName.basics T.never  M.never  A.empty



-- WEBGL TYPES


{-# NOINLINE vec2    #-}; vec2    :: Type; vec2    = AppN ModuleName.vector2 T.vec2    M.vec2    A.empty
{-# NOINLINE vec3    #-}; vec3    :: Type; vec3    = AppN ModuleName.vector3 T.vec3    M.vec3    A.empty
{-# NOINLINE vec4    #-}; vec4    :: Type; vec4    = AppN ModuleName.vector4 T.vec4    M.vec4    A.empty
{-# NOINLINE mat4    #-}; mat4    :: Type; mat4    = AppN ModuleName.matrix4 T.mat4    M.mat4    A.empty
{-# NOINLINE texture #-}; texture :: Type; texture = AppN ModuleName.texture T.texture M.texture A.empty



-- TO TYPE ANNOTATION


toAnnotation :: Variable -> IO I.Annotation
toAnnotation variable =
  run variable variableToType I.Forall


variableToType :: Variable -> Namer I.Type
variableToType variable =
  do  (Descriptor content _ _ _) <- io $ UF.get variable
      case content of
        Structure        t -> termToType t
        Flex             v -> getFlex variable v (\_ i -> I.TVar i)
        Rigid            v -> I.TVar <$> getRigid v
        Alias h n m xs t _ -> alias h n m xs t
        Error              -> $(Crash.crash 'variableToType) "cannot handle Error types"
  where
    alias h n mask args aliasedType =
      do  iargs <- traverseNamer variableToType args
          return (I.TAlias h n mask iargs aliasedType)


termToType :: FlatType -> Namer I.Type
termToType term =
  case term of
    Fun1 a b       -> goFunction <$> variableToType a <*> variableToType b
    App1 h n m xs  -> I.TType h n m <$> traverseNamer variableToType xs
    Unit1          -> return I.TUnit
    Empty1         -> return I.TEmpty
    Pair1   a b    -> I.TPair   <$> variableToType a <*> variableToType b
    Triple1 a b c  -> I.TTriple <$> variableToType a <*> variableToType b <*> variableToType c
    Record1 o fs   -> I.TRecord . F.Fields o <$> NEM.traverse variableToType fs
    ExtRec1 o fs e -> goExtRec  . F.Fields o <$> NEM.traverse variableToType fs <*> variableToType e
  where
    goFunction arg result =
      case result of
        I.TLambda x y zs   -> I.TLambda arg x (y:zs)
        I.TVar _           -> I.TLambda arg result []
        I.TType  _ _ _ _   -> I.TLambda arg result []
        I.TAlias _ _ _ _ _ -> I.TLambda arg result []
        I.TUnit            -> I.TLambda arg result []
        I.TPair _ _        -> I.TLambda arg result []
        I.TTriple _ _ _    -> I.TLambda arg result []
        I.TEmpty           -> I.TLambda arg result []
        I.TRecord _        -> I.TLambda arg result []
        I.TExtRec _ _      -> I.TLambda arg result []

    goExtRec fields ext =
      case ext of
        I.TEmpty            -> I.TRecord fields
        I.TRecord fs        -> I.TRecord (F.extend fields fs)
        I.TExtRec fs i      -> I.TExtRec (F.extend fields fs) i
        I.TVar i            -> I.TExtRec fields i
        I.TAlias _ _ _ xs t -> goExtRec fields (I.dealias xs t)
        I.TType _ _ _ _     -> $(Crash.crash 'termToType) "ran into invalid record extension (Custom Type)"
        I.TLambda _ _ _     -> $(Crash.crash 'termToType) "ran into invalid record extension (Lambda)"
        I.TUnit             -> $(Crash.crash 'termToType) "ran into invalid record extension (Unit)"
        I.TPair _ _         -> $(Crash.crash 'termToType) "ran into invalid record extension (Pair)"
        I.TTriple _ _ _     -> $(Crash.crash 'termToType) "ran into invalid record extension (Triple)"



-- TO ERROR TYPE


toErrorType :: Variable -> IO Err.Type
toErrorType variable =
  run variable variableToErrorType (\_ tipe -> tipe)


variableToErrorType :: Variable -> Namer Err.Type
variableToErrorType variable =
  do  descriptor <- io $ UF.get variable
      let mark = _mark descriptor
      if mark == occursMark
        then return Err.Infinite
        else
          do  io $ UF.modify variable (\desc -> desc { _mark = occursMark })
              errType <- contentToErrorType variable (_content descriptor)
              io $ UF.modify variable (\desc -> desc { _mark = mark })
              return errType


contentToErrorType :: Variable -> Content -> Namer Err.Type
contentToErrorType variable content =
  case content of
    Structure        t -> termToErrorType t
    Flex             v -> getFlex variable v (\w _ -> Err.Flex w)
    Rigid            v -> return (Err.Rigid v)
    Alias h n m xs _ v -> alias h n m xs v
    Error              -> return Err.Error
  where
    alias h n mask args dealiasedVar =
      do  errArgs <- traverseNamer variableToErrorType args
          errType <- variableToErrorType dealiasedVar
          return (Err.Alias h n mask (A.toList errArgs) errType)


termToErrorType :: FlatType -> Namer Err.Type
termToErrorType term =
  case term of
    App1 h n m xs  -> Err.Type h n m . A.toList <$> traverseNamer variableToErrorType xs
    Fun1 a b       -> flattenErrorFunction <$> variableToErrorType a <*> variableToErrorType b
    Unit1          -> return Err.Unit
    Pair1   a b    -> Err.Pair   <$> variableToErrorType a <*> variableToErrorType b
    Triple1 a b c  -> Err.Triple <$> variableToErrorType a <*> variableToErrorType b <*> variableToErrorType c
    Empty1         -> return Err.Empty
    Record1 o fs   -> Err.Record . F.Fields o <$> NEM.traverse variableToErrorType fs
    ExtRec1 o fs e -> flattenErrorRecord . F.Fields o <$> NEM.traverse variableToErrorType fs <*> variableToErrorType e


flattenErrorFunction :: Err.Type -> Err.Type -> Err.Type
flattenErrorFunction arg result =
  case result of
    Err.Lambda x y zs   -> Err.Lambda arg x (y:zs)
    Err.Flex _          -> func
    Err.Rigid _         -> func
    Err.Error           -> func
    Err.Infinite        -> func
    Err.Type _ _ _ _    -> func
    Err.Alias _ _ _ _ _ -> func
    Err.Unit            -> func
    Err.Pair _ _        -> func
    Err.Triple _ _ _    -> func
    Err.Empty           -> func
    Err.Record _        -> func
    Err.ExtRec _ _      -> func
  where
    func = Err.Lambda arg result []


flattenErrorRecord :: F.Fields Err.Type -> Err.Type -> Err.Type
flattenErrorRecord fields ext =
  case ext of
    Err.Empty           -> Err.Record fields
    Err.Record fs       -> Err.Record (F.extend fields fs)
    Err.ExtRec fs e     -> Err.ExtRec (F.extend fields fs) e
    Err.Flex  v         -> Err.ExtRec fields (Err.ExtFlex  v)
    Err.Rigid v         -> Err.ExtRec fields (Err.ExtRigid v)
    Err.Alias _ _ _ _ t -> flattenErrorRecord fields t
    Err.Error           -> Err.Error
    Err.Infinite        -> Err.Error -- TODO handle infinite record extensions better
    Err.Lambda _ _ _    -> $(Crash.crash 'flattenErrorRecord) "ran into invalid record extension (Lambda)"
    Err.Type _ _ _ _    -> $(Crash.crash 'flattenErrorRecord) "ran into invalid record extension (Type)"
    Err.Unit            -> $(Crash.crash 'flattenErrorRecord) "ran into invalid record extension (Unit)"
    Err.Pair _ _        -> $(Crash.crash 'flattenErrorRecord) "ran into invalid record extension (Pair)"
    Err.Triple _ _ _    -> $(Crash.crash 'flattenErrorRecord) "ran into invalid record extension (Triple)"



-- TO UNIQUES
--
-- Gather all of the type variable names available from type annotations. If
-- there is name overlap, add an index to the later names. This also mutates
-- the underlying type to refer to the newly unique names.
--
-- Unnamed variables are skipped in this phase. Their unique names are assigned
-- later to ensure that the user suggestions take precedence.


toUniques :: Variable -> Map.Map T.Var Variable -> IO (Map.Map T.Var Variable)
toUniques var seen =
  do  (Descriptor content rank mark copy) <- UF.get var
      if mark == getVarNamesMark
        then return seen
        else
        do  UF.set var (Descriptor content rank getVarNamesMark copy)
            case content of
              Error              -> return seen
              Flex             x -> toUnique Flex  x var seen
              Rigid            x -> toUnique Rigid x var seen
              Alias _ _ _ xs _ _ -> A.foldlM toUniques seen xs
              Structure flatType ->
                case flatType of
                  App1 _ _ _ xs  -> A.foldlM toUniques seen xs
                  Fun1 arg body  -> toUniques arg =<< toUniques body seen
                  Unit1          -> return seen
                  Pair1   a b    -> toUniques a =<< toUniques b seen
                  Triple1 a b c  -> toUniques a =<< toUniques b =<< toUniques c seen
                  Empty1         -> return seen
                  Record1 _ fs   -> NEM.foldrM toUniques seen fs
                  ExtRec1 _ fs e -> toUniques e =<< NEM.foldrM toUniques seen fs


toUnique :: (T.Var -> Content) -> T.Var -> Variable -> Map.Map T.Var Variable -> IO (Map.Map T.Var Variable)
toUnique mkContent var@(T.Var name con) variable seen =
  if S.isEmpty name
  then return seen
  else
    case Map.lookup var seen of
      Nothing ->
        return $ Map.insert var variable seen

      Just otherVar ->
        do  same <- UF.equivalent variable otherVar
            if same
              then return seen
              else loop 1
  where
    loop index =
      let
        indexed = T.Var (T.genIndexed name index) con
      in
      case Map.lookup indexed seen of
        Nothing ->
          do  UF.modify variable $ \(Descriptor _ rank mark copy) ->
                Descriptor (mkContent indexed) rank mark copy
              return $ Map.insert indexed variable seen

        Just otherVar ->
          do  same <- UF.equivalent variable otherVar
              if same
                then return seen
                else loop (index + 1)



-- NAMER


newtype Namer a =
  Namer ( forall result. State -> (State -> a -> IO result) -> IO result )


data State =
  State
    { _registry :: T.Registry T.Var
    , _counts :: MutableByteArray# RealWorld
    }


run :: Variable -> (Variable -> Namer t) -> (A.Array T.Var -> t -> result) -> IO result
run variable toNamer toResult =
  do  seen  <- toUniques variable Map.empty
      state <- initState seen
      case toNamer variable of
        Namer k ->
          k state $ \(State vars _) tipe ->
            return (toResult (T.toArray vars) tipe)


initState :: Map.Map T.Var Variable -> IO State
initState seen =
  IO $ \s0 ->
    case newByteArray# 5#                  s0 of { (# s1, mba #) ->
    case writeWord64Array# mba 0# 0#Word64 s1 of {    s2         ->
    case writeWord64Array# mba 1# 0#Word64 s2 of {    s3         ->
      (# s3, State (T.fromDict seen) mba #)
  }}}


io :: IO a -> Namer a
io task =
  Namer $ \state ok -> ok state =<< task



-- GET RIGID
--
-- The name is guaranteed to be present from the earlier toUniques pass, so just
-- return the registry index.


getRigid :: T.Var -> Namer T.Index
getRigid var =
  Namer $ \state@(State registry _) ok ->
    ok state (T.lookup var registry)



-- GET FLEX
--
-- Some flex variables are unnamed. Assign these variables unique names, add
-- them to the registry, and mutate the underlying type to point to the new
-- name.


getFlex :: Variable -> T.Var -> (T.Var -> T.Index -> a) -> Namer a
getFlex variable var@(T.Var name con) mk =
  case con of
    T.Any        -> register 0# T.genAny
    T.Comparable -> register 1# T.genComparable
    T.Appendable -> register 2# T.genAppendable
    T.CompAppend -> register 3# T.genCompAppend
    T.Number     -> register 4# T.genNumber
  where
    register offset gen =
      if S.isEmpty name
      then
        Namer $ \(State seen counts) ok ->
          do  i <- readWord8Array offset counts
              let !(I# j, namedVar, index, newSeen) = T.registerNext gen i seen
              writeWord8Array offset j counts
              UF.modify variable (\desc -> desc { _content = Flex var })
              ok (State newSeen counts) (mk namedVar index)
      else
        Namer $ \state@(State registry _) ok ->
          ok state (mk var (T.lookup var registry))


readWord8Array :: Int# -> MutableByteArray# RealWorld -> IO Int
readWord8Array offset mba =
  IO $ \s0 ->
    case readWord8Array# mba offset s0 of
      (# s1, w #) ->
        (# s1, I# (word2Int# (word8ToWord# w)) #)


writeWord8Array :: Int# -> Int# -> MutableByteArray# RealWorld -> IO ()
writeWord8Array offset i mba =
  IO $ \s0 ->
    case writeWord8Array# mba offset (wordToWord8# (int2Word# i)) s0 of
      s1 ->
        (# s1, () #)



-- NAMER INSTANCES


instance Functor Namer where
  {-# INLINE fmap #-}
  fmap func (Namer k) =
    Namer $ \state ok ->
      let
        ok' s arg = ok s (func arg)
      in
      k state ok'


instance Applicative Namer where
  {-# INLINE pure #-}
  pure value =
    Namer $ \state ok ->
      ok state value

  {-# INLINE (<*>) #-}
  (<*>) (Namer kFunc) (Namer kArg) =
    Namer $ \state ok ->
      let
        okF sF func =
          let
            okA s arg = ok s (func arg)
          in
          kArg sF okA
      in
      kFunc state okF


instance Monad Namer where
  {-# INLINE (>>=) #-}
  (>>=) (Namer kA) callback =
    Namer $ \state ok ->
      let
        okA s value =
          case callback value of
            Namer kB -> kB s ok
      in
      kA state okA



-- TRAVERSE NAMER


traverseNamer :: (a -> Namer b) -> A.Array a -> Namer (A.Array b)
traverseNamer func (A.Array sa#) =
  let
    !len = sizeofSmallArray# sa#
  in
  if isTrue# (len ==# 0#)
  then return A.empty
  else
    case indexSmallArray# sa# 0# of
      (# a #) ->
        do  sma <- new len =<< func a
            traverseNamer# func sa# sma len


traverseNamer# :: (a -> Namer b) -> SmallArray# a -> SMA b -> Int# -> Namer (A.Array b)
traverseNamer# func sa# (SMA sma) len =
    go 1#
  where
    go i =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa# i of
          (# a #) ->
            do  write sma i =<< func a
                go (i +# 1#)
      else
        io $ IO $ \s ->
          case unsafeFreezeSmallArray# sma s of
            (# s1, sa1# #) ->
              (# s1, A.Array sa1# #)


data SMA a =
  SMA (SmallMutableArray# RealWorld a)


new :: Int# -> a -> Namer (SMA a)
new len a =
  io $ IO $ \s ->
    case newSmallArray# len a s of
      (# s1, sma #) ->
        (# s1, SMA sma #)


write :: SmallMutableArray# RealWorld a -> Int# -> a -> Namer ()
write sma i a =
  io $ IO $ \s ->
    case writeSmallArray# sma i a s of
      s1 ->
        (# s1, () #)

