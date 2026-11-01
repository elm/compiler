{-# LANGUAGE ExtendedLiterals, MagicHash #-}
module AST.Optimized
  ( Def(..)
  , Expr(..)
  , Global(..)
  , TailAssign(..)
  , func1
  , call1
  , call2
  --
  , Path(..)
  , Destructor(..)
  , Decider(..)
  , Choice(..)
  , GlobalGraph(..)
  , LocalGraph(..)
  , Main(..)
  , Node(..)
  , EffectsType(..)
  , empty
  , addGlobalGraph
  , addLocalGraph
  , addKernel
  , toKernelGlobal
  --
  , eGlobalGraph, dGlobalGraph
  , eLocalGraph, dLocalGraph
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4, liftM5)
import qualified Data.Map as Map
import qualified Data.NonEmptyMap as NEM
import qualified Data.Set as Set
import GHC.Int (Int(..))
import GHC.Prim

import qualified Array as A
import qualified Bytes.Decode as D
import qualified Bytes.Encode as E

import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Shader as Shader
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeVar as T
import qualified Data.Index as Index
import qualified Elm.Float as EF
import qualified Elm.Interface as I
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.String as ES
import qualified Nitpick.Utils as Nit
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Annotation as A



-- EXPRESSIONS


data Expr
  = Bool Bool
  | Chr Char
  | Str ES.String
  | Int Int
  | Float EF.Float
  | VarLocal N.Name
  | VarGlobal Global
  | VarEnum Global Tag.Tag
  | VarBox Global
  | VarCycle ModuleName.Canonical N.Name
  | VarDebug N.Name ModuleName.Canonical A.Region (Maybe N.Name)
  | VarKernel Module.Kernel N.Name
  | List [Expr]
  | Function N.Name [N.Name] Expr
  | Call Expr Expr [Expr]
  | TailCall N.Name TailAssign [TailAssign]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | Destruct Destructor Expr
  | Case N.Name N.Name (Decider Choice) [(Int, Expr)]
  | Accessor N.Name
  | Access Expr N.Name
  | Update Expr (NEM.Map N.Name Expr)
  | Record (NEM.Map N.Name Expr)
  | Empty
  | Unit
  | Pair Expr Expr
  | Triple Expr Expr Expr
  | Shader Shader.Source (Set.Set N.Name) (Set.Set N.Name)


data Global = Global ModuleName.Canonical N.Name


data TailAssign =
  TailAssign N.Name Expr


func1 :: N.Name -> Expr -> Expr
func1 x e =
  Function x [] e


call1 :: Expr -> Expr -> Expr
call1 func x =
  Call func x []


call2 :: Expr -> Expr -> Expr -> Expr
call2 func x y =
  Call func x [y]



-- DEFINITIONS


data Def
  = Def N.Name Expr
  | TailDef N.Name N.Name [N.Name] Expr


data Destructor =
  Destructor N.Name Path


data Path
  = Index Index.ZeroBased Path
  | Field N.Name Path
  | Unbox Path
  | Root N.Name



-- BRANCHING


data Decider a
  = Leaf a
  | Chain
      { _testChain :: [(DT.Path, DT.Test)]
      , _success :: Decider a
      , _failure :: Decider a
      }
  | FanOut
      { _path :: DT.Path
      , _tests :: [(DT.Test, Decider a)]
      , _fallback :: Decider a
      }
  deriving (Eq)


data Choice
  = Inline Expr
  | Jump Int



-- OBJECT GRAPH


data GlobalGraph =
  GlobalGraph
    { _g_nodes :: Map.Map Global Node
    , _g_fields :: Map.Map N.Name Int
    }


data LocalGraph =
  LocalGraph
    { _l_main :: Maybe Main
    , _l_nodes :: Map.Map Global Node  -- PERF profile switching Global to Name
    , _l_fields :: Map.Map N.Name Int
    }


data Main
  = Static
  | Dynamic
      { _msg_args :: A.Array T.Var
      , _message  :: I.Type
      , _decoder  :: Expr
      }


data Node
  = Define Expr (Set.Set Global)
  | DefineTailFunc N.Name [N.Name] Expr (Set.Set Global)
  | Variant Tag.Tag Int
  | Enum Tag.Tag
  | Vector Int
  | Box
  | Void
  | Link Global
  | Cycle N.Name [N.Name] [(N.Name, Expr)] [Def] (Set.Set Global)
  | Manager EffectsType
  | Kernel [K.Chunk] (Set.Set Global)
  | PortIncoming Expr (Set.Set Global)
  | PortOutgoing Expr (Set.Set Global)


data EffectsType = Cmd | Sub | Fx



-- GRAPHS


{-# NOINLINE empty #-}
empty :: GlobalGraph
empty =
  GlobalGraph Map.empty Map.empty


addGlobalGraph :: GlobalGraph -> GlobalGraph -> GlobalGraph
addGlobalGraph (GlobalGraph nodes1 fields1) (GlobalGraph nodes2 fields2) =
  GlobalGraph
    { _g_nodes = Map.union nodes1 nodes2
    , _g_fields = Map.union fields1 fields2
    }


addLocalGraph :: LocalGraph -> GlobalGraph -> GlobalGraph
addLocalGraph (LocalGraph _ nodes1 fields1) (GlobalGraph nodes2 fields2) =
  GlobalGraph
    { _g_nodes = Map.union nodes1 nodes2
    , _g_fields = Map.union fields1 fields2
    }


addKernel :: Module.Kernel -> [K.Chunk] -> GlobalGraph -> GlobalGraph
addKernel kname chunks (GlobalGraph nodes fields) =
  let
    global = toKernelGlobal kname
    node = Kernel chunks (foldr addKernelDep Set.empty chunks)
  in
  GlobalGraph
    { _g_nodes = Map.insert global node nodes
    , _g_fields = Map.union (K.countFields chunks) fields
    }


addKernelDep :: K.Chunk -> Set.Set Global -> Set.Set Global
addKernelDep chunk deps =
  case chunk of
    K.JS _       -> deps
    K.ElmVar h n -> Set.insert (Global h n) deps
    K.JsVar  k _ -> Set.insert (toKernelGlobal k) deps
    K.ElmField _ -> deps
    K.JsField _  -> deps
    K.JsEnum _   -> deps
    K.Debug      -> deps
    K.Prod       -> deps


toKernelGlobal :: Module.Kernel -> Global
toKernelGlobal kname =
    Global (ModuleName.Canonical Pkg.kernel name) N.unit
  where
    name = Module.fromString (Module.kernelToString kname)



-- INSTANCES


instance Eq Global where
  (==) (Global home1 name1) (Global home2 name2) =
    name1 == name2 && home1 == home2


instance Ord Global where
  compare (Global home1 name1) (Global home2 name2) =
    case compare name1 name2 of
      LT -> LT
      EQ -> compare home1 home2
      GT -> GT



-- BINARY


eGlobal :: Global -> E.Builder
eGlobal (Global h n) =
  ModuleName.eCanonical h <> N.encode n


dGlobal :: D.Decoder Global
dGlobal =
  liftM2 Global ModuleName.dCanonical N.decode


eExpr :: Expr -> E.Builder
eExpr expr =
  case expr of
    Bool b           -> E.u8#  0#Word8 <> E.bool b
    Chr c            -> E.u8#  1#Word8 <> E.char c
    Str s            -> E.u8#  2#Word8 <> ES.encode s
    Int i            -> E.u8#  3#Word8 <> E.int i
    Float f          -> E.u8#  4#Word8 <> EF.encode f
    VarLocal x       -> E.u8#  5#Word8 <> N.encode x
    VarGlobal g      -> E.u8#  6#Word8 <> eGlobal g
    VarEnum g t      -> E.u8#  7#Word8 <> eGlobal g <> Tag.eTag t
    VarBox g         -> E.u8#  8#Word8 <> eGlobal g
    VarCycle h n     -> E.u8#  9#Word8 <> ModuleName.eCanonical h <> N.encode n
    VarDebug n h r m -> E.u8# 10#Word8 <> N.encode n <> ModuleName.eCanonical h <> A.eRegion r <> E.maybe N.encode m
    VarKernel h n    -> E.u8# 11#Word8 <> Module.eKernel h <> N.encode n
    List es          -> E.u8# 12#Word8 <> E.list32 eExpr es
    Function x xs b  -> E.u8# 13#Word8 <> N.encode x <> E.list32 N.encode xs <> eExpr b
    Call f x xs      -> E.u8# 14#Word8 <> eExpr f <> eExpr x <> E.list32 eExpr xs
    TailCall f x xs  -> E.u8# 15#Word8 <> N.encode f <> eTailAssign x <> E.list32 eTailAssign xs
    If bs f          -> E.u8# 16#Word8 <> E.list32 (\(c,b) -> eExpr c <> eExpr b) bs <> eExpr f
    Let d e          -> E.u8# 17#Word8 <> eDef d <> eExpr e
    Destruct d e     -> E.u8# 18#Word8 <> eDestructor d <> eExpr e
    Case a b c d     -> E.u8# 19#Word8 <> N.encode a <> N.encode b <> eDecider eChoice c <> E.list32 (\(i,e) -> E.int i <> eExpr e) d
    Accessor f       -> E.u8# 20#Word8 <> N.encode f
    Access e f       -> E.u8# 21#Word8 <> eExpr e <> N.encode f
    Update e fs      -> E.u8# 22#Word8 <> eExpr e <> NEM.encode32 N.encode eExpr fs
    Record fs        -> E.u8# 23#Word8 <> NEM.encode32 N.encode eExpr fs
    Empty            -> E.u8# 24#Word8
    Unit             -> E.u8# 25#Word8
    Pair   a b       -> E.u8# 26#Word8 <> eExpr a <> eExpr b
    Triple a b c     -> E.u8# 27#Word8 <> eExpr a <> eExpr b <> eExpr c
    Shader s a u     -> E.u8# 28#Word8 <> Shader.eSource s <> E.set32 N.encode a <> E.set32 N.encode u


dExpr :: D.Decoder Expr
dExpr =
  do  tag <- D.u8
      case tag of
        0  -> liftM  Bool D.bool
        1  -> liftM  Chr D.char
        2  -> liftM  Str ES.decode
        3  -> liftM  Int D.int
        4  -> liftM  Float EF.decode
        5  -> liftM  VarLocal N.decode
        6  -> liftM  VarGlobal dGlobal
        7  -> liftM2 VarEnum dGlobal Tag.dTag
        8  -> liftM  VarBox dGlobal
        9  -> liftM2 VarCycle ModuleName.dCanonical N.decode
        10 -> liftM4 VarDebug N.decode ModuleName.dCanonical A.dRegion (D.maybe N.decode)
        11 -> liftM2 VarKernel Module.dKernel N.decode
        12 -> liftM  List (D.list32 dExpr)
        13 -> liftM3 Function N.decode (D.list32 N.decode) dExpr
        14 -> liftM3 Call dExpr dExpr (D.list32 dExpr)
        15 -> liftM3 TailCall N.decode dTailAssign (D.list32 dTailAssign)
        16 -> liftM2 If (D.list32 (liftM2 (,) dExpr dExpr)) dExpr
        17 -> liftM2 Let dDef dExpr
        18 -> liftM2 Destruct dDestructor dExpr
        19 -> liftM4 Case N.decode N.decode (dDecider dChoice) (D.list32 (liftM2 (,) D.int dExpr))
        20 -> liftM  Accessor N.decode
        21 -> liftM2 Access dExpr N.decode
        22 -> liftM2 Update dExpr (NEM.decode32 N.decode dExpr)
        23 -> liftM  Record (NEM.decode32 N.decode dExpr)
        24 -> pure   Empty
        25 -> pure   Unit
        26 -> liftM2 Pair dExpr dExpr
        27 -> liftM3 Triple dExpr dExpr dExpr
        28 -> liftM3 Shader Shader.dSource (D.set32 N.decode) (D.set32 N.decode)
        _  -> D.expecting "Expr"


eTailAssign :: TailAssign -> E.Builder
eTailAssign (TailAssign x e) =
  N.encode x <> eExpr e


dTailAssign :: D.Decoder TailAssign
dTailAssign =
  liftM2 TailAssign N.decode dExpr


eDef :: Def -> E.Builder
eDef def =
  case def of
    Def     n      e -> E.u8# 0#Word8 <> N.encode n <> eExpr e
    TailDef n x xs e -> E.u8# 1#Word8 <> N.encode n <> N.encode x <> E.list32 N.encode xs <> eExpr e


dDef :: D.Decoder Def
dDef =
  do  tag <- D.u8
      case tag of
        0 -> liftM2 Def N.decode dExpr
        1 -> liftM4 TailDef N.decode N.decode (D.list32 N.decode) dExpr
        _ -> D.expecting "Def"


eDestructor :: Destructor -> E.Builder
eDestructor (Destructor n p) =
  N.encode n <> ePath p


dDestructor :: D.Decoder Destructor
dDestructor =
  liftM2 Destructor N.decode dPath


ePath :: Path -> E.Builder
ePath path =
  case path of
    Index i p -> E.u8# 0#Word8 <> Index.eZeroBased i <> ePath p
    Field f p -> E.u8# 1#Word8 <> N.encode f <> ePath p
    Unbox p   -> E.u8# 2#Word8 <> ePath p
    Root n    -> E.u8# 3#Word8 <> N.encode n


dPath :: D.Decoder Path
dPath =
  do  tag <- D.u8
      case tag of
        0 -> liftM2 Index Index.dZeroBased dPath
        1 -> liftM2 Field N.decode dPath
        2 -> liftM  Unbox dPath
        3 -> liftM  Root N.decode
        _ -> D.expecting "Path"


eDecider :: (a -> E.Builder) -> Decider a -> E.Builder
eDecider enc =
    go
  where
    go decider =
      case decider of
        Leaf   a     -> E.u8# 0#Word8 <> enc a
        Chain  c s f -> E.u8# 1#Word8 <> E.list32 (\(p,t) -> ePath_ p <> eTest t) c <> go s <> go f
        FanOut p b f -> E.u8# 2#Word8 <> ePath_ p <> E.list32 (\(t,d) -> eTest t <> go d) b <> go f


dDecider :: D.Decoder a -> D.Decoder (Decider a)
dDecider dec =
    go
  where
    go =
      do  tag <- D.u8
          case tag of
            0 -> liftM  Leaf dec
            1 -> liftM3 Chain (D.list32 (liftM2 (,) dPath_ dTest)) go go
            2 -> liftM3 FanOut dPath_ (D.list32 (liftM2 (,) dTest go)) go
            _ -> D.expecting "Decider"


eChoice :: Choice -> E.Builder
eChoice choice =
  case choice of
    Inline e -> E.u8# 0#Word8 <> eExpr e
    Jump   i -> E.u8# 1#Word8 <> E.int i


dChoice :: D.Decoder Choice
dChoice =
  do  tag <- D.u8
      case tag of
        0 -> liftM Inline dExpr
        1 -> liftM Jump D.int
        _ -> D.expecting "Choice"



-- DECISION TREE


eTest :: DT.Test -> E.Builder
eTest test =
  case test of
    DT.IsVariant h n t a -> E.u8# 0#Word8 <> ModuleName.eCanonical h <> N.encode n <> Tag.eTag t <> eArity a
    DT.IsEnum    h n t a -> E.u8# 1#Word8 <> ModuleName.eCanonical h <> N.encode n <> Tag.eTag t <> eArity a
    DT.IsCons            -> E.u8# 2#Word8
    DT.IsNil             -> E.u8# 3#Word8
    DT.IsChr c           -> E.u8# 4#Word8 <> E.char c
    DT.IsStr s           -> E.u8# 5#Word8 <> ES.encode s
    DT.IsInt i           -> E.u8# 6#Word8 <> E.int i
    DT.IsBool b          -> E.u8# 7#Word8 <> E.bool b


dTest :: D.Decoder DT.Test
dTest =
  do  tag <- D.u8
      case tag of
        0 -> liftM4 DT.IsVariant ModuleName.dCanonical N.decode Tag.dTag dArity
        1 -> liftM4 DT.IsEnum    ModuleName.dCanonical N.decode Tag.dTag dArity
        2 -> pure   DT.IsCons
        3 -> pure   DT.IsNil
        4 -> liftM  DT.IsChr D.char
        5 -> liftM  DT.IsStr ES.decode
        6 -> liftM  DT.IsInt D.int
        7 -> liftM  DT.IsBool D.bool
        _ -> D.expecting "DecisionTree.Test"


ePath_ :: DT.Path -> E.Builder
ePath_ path =
  case path of
    DT.Index i p -> E.u8# 0#Word8 <> Index.eZeroBased i <> ePath_ p
    DT.Unbox   p -> E.u8# 1#Word8 <> ePath_ p
    DT.Empty     -> E.u8# 2#Word8


dPath_ :: D.Decoder DT.Path
dPath_ =
  do  tag <- D.u8
      case tag of
        0 -> liftM2 DT.Index Index.dZeroBased dPath_
        1 -> liftM  DT.Unbox dPath_
        2 -> pure   DT.Empty
        _ -> D.expecting "DecisionTree.Path"



-- ARITY


eArity :: Nit.NumAlts -> E.Builder
eArity (Nit.NumAlts n) =
  E.u32 (fromIntegral n)


dArity :: D.Decoder Nit.NumAlts
dArity =
  D.u32# (\n -> Nit.NumAlts (I# (word2Int# (word32ToWord# n))))



-- OBJECT GRAPH


eGlobalGraph :: GlobalGraph -> E.Builder
eGlobalGraph (GlobalGraph n f) =
  E.dict32 eGlobal eNode n <> E.dict32 N.encode E.int f


dGlobalGraph :: D.Decoder GlobalGraph
dGlobalGraph =
  liftM2 GlobalGraph (D.dict32 dGlobal dNode) (D.dict32 N.decode D.int)


eLocalGraph :: LocalGraph -> E.Builder
eLocalGraph (LocalGraph m n f) =
  E.maybe eMain m <> E.dict32 eGlobal eNode n <> E.dict32 N.encode E.int f


dLocalGraph :: D.Decoder LocalGraph
dLocalGraph =
  liftM3 LocalGraph (D.maybe dMain) (D.dict32 dGlobal dNode) (D.dict32 N.decode D.int)


eMain :: Main -> E.Builder
eMain main =
  case main of
    Static         -> E.u8# 0#Word8
    Dynamic xs t e -> E.u8# 1#Word8 <> E.array8 T.eVar xs <> I.eType t <> eExpr e


dMain :: D.Decoder Main
dMain =
    do  tag <- D.u8
        case tag of
          0 -> return Static
          1 -> liftM3 Dynamic (D.array8 T.dVar) I.dType dExpr
          _ -> D.expecting "Main"


eNode :: Node -> E.Builder
eNode node =
    case node of
      Define e d              -> E.u8#  0#Word8 <> eExpr e <> E.set32 eGlobal d
      DefineTailFunc x xs e d -> E.u8#  1#Word8 <> N.encode x <> E.list32 N.encode xs <> eExpr e <> E.set32 eGlobal d
      Variant t a             -> E.u8#  2#Word8 <> Tag.eTag t <> E.int a
      Enum t                  -> E.u8#  3#Word8 <> Tag.eTag t
      Vector a                -> E.u8#  4#Word8 <> E.int a
      Box                     -> E.u8#  5#Word8
      Void                    -> E.u8#  6#Word8
      Link g                  -> E.u8#  7#Word8 <> eGlobal g
      Cycle n ns v f d        -> E.u8#  8#Word8 <> N.encode n <> E.list32 N.encode ns <> E.list32 (\(x,e) -> N.encode x <> eExpr e) v <> E.list32 eDef f <> E.set32 eGlobal d
      Manager t               -> E.u8#  9#Word8 <> eEffectsType t
      Kernel cs d             -> E.u8# 10#Word8 <> E.list32 K.eChunk cs <> E.set32 eGlobal d
      PortIncoming e d        -> E.u8# 11#Word8 <> eExpr e <> E.set32 eGlobal d
      PortOutgoing e d        -> E.u8# 12#Word8 <> eExpr e <> E.set32 eGlobal d


dNode :: D.Decoder Node
dNode =
  do  tag <- D.u8
      case tag of
        0  -> liftM2 Define dExpr (D.set32 dGlobal)
        1  -> liftM4 DefineTailFunc N.decode (D.list32 N.decode) dExpr (D.set32 dGlobal)
        2  -> liftM2 Variant Tag.dTag D.int
        3  -> liftM  Enum Tag.dTag
        4  -> liftM  Vector D.int
        5  -> pure Box
        6  -> pure Void
        7  -> liftM  Link dGlobal
        8  -> liftM5 Cycle N.decode (D.list32 N.decode) (D.list32 (liftM2 (,) N.decode dExpr)) (D.list32 dDef) (D.set32 dGlobal)
        9  -> liftM  Manager dEffectsType
        10 -> liftM2 Kernel (D.list32 K.dChunk) (D.set32 dGlobal)
        11 -> liftM2 PortIncoming dExpr (D.set32 dGlobal)
        12 -> liftM2 PortOutgoing dExpr (D.set32 dGlobal)
        _  -> D.expecting "Node"


eEffectsType :: EffectsType -> E.Builder
eEffectsType effectsType =
  case effectsType of
    Cmd -> E.u8# 0#Word8
    Sub -> E.u8# 1#Word8
    Fx  -> E.u8# 2#Word8


dEffectsType :: D.Decoder EffectsType
dEffectsType =
  do  tag <- D.u8
      case tag of
        0 -> return Cmd
        1 -> return Sub
        2 -> return Fx
        _ -> D.expecting "EffectsType"
