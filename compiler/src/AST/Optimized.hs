{-# LANGUAGE ExtendedLiterals, MagicHash #-}
module AST.Optimized
  ( Def(..)
  , Expr(..)
  , Global(..)
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


import Control.Monad (liftM, liftM2, liftM3, liftM4)
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Name (Name)
import qualified Data.Set as Set

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E

import qualified AST.Canonical as Can
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Float as EF
import qualified Elm.Interface as I
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.String as ES
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Annotation as A



-- EXPRESSIONS


data Expr
  = Bool Bool
  | Chr Char
  | Str ES.String
  | Int Int
  | Float EF.Float
  | VarLocal Name
  | VarGlobal Global
  | VarEnum Global Index.ZeroBased
  | VarBox Global
  | VarCycle ModuleName.Canonical Name
  | VarDebug Name ModuleName.Canonical A.Region (Maybe Name)
  | VarKernel Name Name
  | List [Expr]
  | Function [Name] Expr
  | Call Expr [Expr]
  | TailCall Name [(Name, Expr)]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | Destruct Destructor Expr
  | Case Name Name (Decider Choice) [(Int, Expr)]
  | Accessor Name
  | Access Expr Name
  | Update Expr (Map.Map Name Expr)
  | Record (Map.Map Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader Shader.Source (Set.Set Name) (Set.Set Name)


data Global = Global ModuleName.Canonical Name



-- DEFINITIONS


data Def
  = Def Name Expr
  | TailDef Name [Name] Expr


data Destructor =
  Destructor Name Path


data Path
  = Index Index.ZeroBased Path
  | Field Name Path
  | Unbox Path
  | Root Name



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
    , _g_fields :: Map.Map Name Int
    }


data LocalGraph =
  LocalGraph
    { _l_main :: Maybe Main
    , _l_nodes :: Map.Map Global Node  -- PERF profile switching Global to Name
    , _l_fields :: Map.Map Name Int
    }


data Main
  = Static
  | Dynamic
      { _message :: Can.Type
      , _decoder :: Expr
      }


data Node
  = Define Expr (Set.Set Global)
  | DefineTailFunc [Name] Expr (Set.Set Global)
  | Ctor Index.ZeroBased Int
  | Enum Index.ZeroBased
  | Box
  | Link Global
  | Cycle [Name] [(Name, Expr)] [Def] (Set.Set Global)
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


addKernel :: Name.Name -> [K.Chunk] -> GlobalGraph -> GlobalGraph
addKernel shortName chunks (GlobalGraph nodes fields) =
  let
    global = toKernelGlobal shortName
    node = Kernel chunks (foldr addKernelDep Set.empty chunks)
  in
  GlobalGraph
    { _g_nodes = Map.insert global node nodes
    , _g_fields = Map.union (K.countFields chunks) fields
    }


addKernelDep :: K.Chunk -> Set.Set Global -> Set.Set Global
addKernelDep chunk deps =
  case chunk of
    K.JS _              -> deps
    K.ElmVar home name  -> Set.insert (Global home name) deps
    K.JsVar shortName _ -> Set.insert (toKernelGlobal shortName) deps
    K.ElmField _        -> deps
    K.JsField _         -> deps
    K.JsEnum _          -> deps
    K.Debug             -> deps
    K.Prod              -> deps


toKernelGlobal :: Name.Name -> Global
toKernelGlobal shortName =
  Global (ModuleName.Canonical Pkg.kernel shortName) Name.dollar



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
  ModuleName.eCanonical h <> Name.encode n


dGlobal :: D.Decoder Global
dGlobal =
  liftM2 Global ModuleName.dCanonical Name.decode


eExpr :: Expr -> E.Builder
eExpr expr =
  case expr of
    Bool b           -> E.u8#  0#Word8 <> E.bool b
    Chr c            -> E.u8#  1#Word8 <> E.char c
    Str s            -> E.u8#  2#Word8 <> ES.encode s
    Int i            -> E.u8#  3#Word8 <> E.int i
    Float f          -> E.u8#  4#Word8 <> EF.encode f
    VarLocal x       -> E.u8#  5#Word8 <> Name.encode x
    VarGlobal g      -> E.u8#  6#Word8 <> eGlobal g
    VarEnum g i      -> E.u8#  7#Word8 <> eGlobal g <> Index.eZeroBased i
    VarBox g         -> E.u8#  8#Word8 <> eGlobal g
    VarCycle h n     -> E.u8#  9#Word8 <> ModuleName.eCanonical h <> Name.encode n
    VarDebug n h r m -> E.u8# 10#Word8 <> Name.encode n <> ModuleName.eCanonical h <> A.eRegion r <> E.maybe Name.encode m
    VarKernel h n    -> E.u8# 11#Word8 <> Name.encode h <> Name.encode n
    List es          -> E.u8# 12#Word8 <> E.list32 eExpr es
    Function xs b    -> E.u8# 13#Word8 <> E.list32 Name.encode xs <> eExpr b
    Call f xs        -> E.u8# 14#Word8 <> eExpr f <> E.list32 eExpr xs
    TailCall f xs    -> E.u8# 15#Word8 <> Name.encode f <> E.list32 (\(x,e) -> Name.encode x <> eExpr e) xs
    If bs f          -> E.u8# 16#Word8 <> E.list32 (\(c,b) -> eExpr c <> eExpr b) bs <> eExpr f
    Let d e          -> E.u8# 17#Word8 <> eDef d <> eExpr e
    Destruct d e     -> E.u8# 18#Word8 <> eDestructor d <> eExpr e
    Case a b c d     -> E.u8# 19#Word8 <> Name.encode a <> Name.encode b <> eDecider eChoice c <> E.list32 (\(i,e) -> E.int i <> eExpr e) d
    Accessor f       -> E.u8# 20#Word8 <> Name.encode f
    Access e f       -> E.u8# 21#Word8 <> eExpr e <> Name.encode f
    Update e fs      -> E.u8# 22#Word8 <> eExpr e <> E.dict32 Name.encode eExpr fs
    Record fs        -> E.u8# 23#Word8 <> E.dict32 Name.encode eExpr fs
    Unit             -> E.u8# 24#Word8
    Tuple a b c      -> E.u8# 25#Word8 <> eExpr a <> eExpr b <> E.maybe eExpr c
    Shader s a u     -> E.u8# 26#Word8 <> Shader.eSource s <> E.set32 Name.encode a <> E.set32 Name.encode u


dExpr :: D.Decoder Expr
dExpr =
  do  tag <- D.u8
      case tag of
        0  -> liftM  Bool D.bool
        1  -> liftM  Chr D.char
        2  -> liftM  Str ES.decode
        3  -> liftM  Int D.int
        4  -> liftM  Float EF.decode
        5  -> liftM  VarLocal Name.decode
        6  -> liftM  VarGlobal dGlobal
        7  -> liftM2 VarEnum dGlobal Index.dZeroBased
        8  -> liftM  VarBox dGlobal
        9  -> liftM2 VarCycle ModuleName.dCanonical Name.decode
        10 -> liftM4 VarDebug Name.decode ModuleName.dCanonical A.dRegion (D.maybe Name.decode)
        11 -> liftM2 VarKernel Name.decode Name.decode
        12 -> liftM  List (D.list32 dExpr)
        13 -> liftM2 Function (D.list32 Name.decode) dExpr
        14 -> liftM2 Call dExpr (D.list32 dExpr)
        15 -> liftM2 TailCall Name.decode (D.list32 (liftM2 (,) Name.decode dExpr))
        16 -> liftM2 If (D.list32 (liftM2 (,) dExpr dExpr)) dExpr
        17 -> liftM2 Let dDef dExpr
        18 -> liftM2 Destruct dDestructor dExpr
        19 -> liftM4 Case Name.decode Name.decode (dDecider dChoice) (D.list32 (liftM2 (,) D.int dExpr))
        20 -> liftM  Accessor Name.decode
        21 -> liftM2 Access dExpr Name.decode
        22 -> liftM2 Update dExpr (D.dict32 Name.decode dExpr)
        23 -> liftM  Record (D.dict32 Name.decode dExpr)
        24 -> pure   Unit
        25 -> liftM3 Tuple dExpr dExpr (D.maybe dExpr)
        26 -> liftM3 Shader Shader.dSource (D.set32 Name.decode) (D.set32 Name.decode)
        _  -> D.expecting "Expr"


eDef :: Def -> E.Builder
eDef def =
  case def of
    Def     n    e -> E.u8# 0#Word8 <> Name.encode n <> eExpr e
    TailDef n xs e -> E.u8# 1#Word8 <> Name.encode n <> E.list32 Name.encode xs <> eExpr e


dDef :: D.Decoder Def
dDef =
  do  tag <- D.u8
      case tag of
        0 -> liftM2 Def Name.decode dExpr
        1 -> liftM3 TailDef Name.decode (D.list32 Name.decode) dExpr
        _ -> D.expecting "Def"



eDestructor :: Destructor -> E.Builder
eDestructor (Destructor n p) =
  Name.encode n <> ePath p


dDestructor :: D.Decoder Destructor
dDestructor =
  liftM2 Destructor Name.decode dPath


ePath :: Path -> E.Builder
ePath path =
  case path of
    Index i p -> E.u8# 0#Word8 <> Index.eZeroBased i <> ePath p
    Field f p -> E.u8# 1#Word8 <> Name.encode f <> ePath p
    Unbox p   -> E.u8# 2#Word8 <> ePath p
    Root n    -> E.u8# 3#Word8 <> Name.encode n


dPath :: D.Decoder Path
dPath =
  do  tag <- D.u8
      case tag of
        0 -> liftM2 Index Index.dZeroBased dPath
        1 -> liftM2 Field Name.decode dPath
        2 -> liftM  Unbox dPath
        3 -> liftM  Root Name.decode
        _ -> D.expecting "Path"


eDecider :: (a -> E.Builder) -> Decider a -> E.Builder
eDecider enc =
    go
  where
    go decider =
      case decider of
        Leaf   a     -> E.u8# 0#Word8 <> enc a
        Chain  c s f -> E.u8# 1#Word8 <> E.list32 (\(p,t) -> DT.ePath p <> DT.eTest t) c <> go s <> go f
        FanOut p b f -> E.u8# 2#Word8 <> DT.ePath p <> E.list32 (\(t,d) -> DT.eTest t <> go d) b <> go f


dDecider :: D.Decoder a -> D.Decoder (Decider a)
dDecider dec =
    go
  where
    go =
      do  tag <- D.u8
          case tag of
            0 -> liftM  Leaf dec
            1 -> liftM3 Chain (D.list32 (liftM2 (,) DT.dPath DT.dTest)) go go
            2 -> liftM3 FanOut DT.dPath (D.list32 (liftM2 (,) DT.dTest go)) go
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



-- OBJECT GRAPH


eGlobalGraph :: GlobalGraph -> E.Builder
eGlobalGraph (GlobalGraph n f) =
  E.dict32 eGlobal eNode n <> E.dict32 Name.encode E.int f


dGlobalGraph :: D.Decoder GlobalGraph
dGlobalGraph =
  liftM2 GlobalGraph (D.dict32 dGlobal dNode) (D.dict32 Name.decode D.int)


eLocalGraph :: LocalGraph -> E.Builder
eLocalGraph (LocalGraph m n f) =
  E.maybe eMain m <> E.dict32 eGlobal eNode n <> E.dict32 Name.encode E.int f


dLocalGraph :: D.Decoder LocalGraph
dLocalGraph =
  liftM3 LocalGraph (D.maybe dMain) (D.dict32 dGlobal dNode) (D.dict32 Name.decode D.int)


eMain :: Main -> E.Builder
eMain main =
  case main of
    Static      -> E.u8# 0#Word8
    Dynamic t e -> E.u8# 1#Word8 <> I.eType t <> eExpr e


dMain :: D.Decoder Main
dMain =
    do  tag <- D.u8
        case tag of
          0 -> return Static
          1 -> liftM2 Dynamic I.dType dExpr
          _ -> D.expecting "Main"


eNode :: Node -> E.Builder
eNode node =
    case node of
      Define e d            -> E.u8#  0#Word8 <> eExpr e <> E.set32 eGlobal d
      DefineTailFunc xs e d -> E.u8#  1#Word8 <> E.list32 Name.encode xs <> eExpr e <> E.set32 eGlobal d
      Ctor i a              -> E.u8#  2#Word8 <> Index.eZeroBased i <> E.int a
      Enum i                -> E.u8#  3#Word8 <> Index.eZeroBased i
      Box                   -> E.u8#  4#Word8
      Link g                -> E.u8#  5#Word8 <> eGlobal g
      Cycle n v f d         -> E.u8#  6#Word8 <> E.list32 Name.encode n <> E.list32 (\(x,e) -> Name.encode x <> eExpr e) v <> E.list32 eDef f <> E.set32 eGlobal d
      Manager t             -> E.u8#  7#Word8 <> eEffectsType t
      Kernel cs d           -> E.u8#  8#Word8 <> E.list32 K.eChunk cs <> E.set32 eGlobal d
      PortIncoming e d      -> E.u8#  9#Word8 <> eExpr e <> E.set32 eGlobal d
      PortOutgoing e d      -> E.u8# 10#Word8 <> eExpr e <> E.set32 eGlobal d


dNode :: D.Decoder Node
dNode =
  do  tag <- D.u8
      case tag of
        0  -> liftM2 Define dExpr (D.set32 dGlobal)
        1  -> liftM3 DefineTailFunc (D.list32 Name.decode) dExpr (D.set32 dGlobal)
        2  -> liftM2 Ctor Index.dZeroBased D.int
        3  -> liftM  Enum Index.dZeroBased
        4  -> return Box
        5  -> liftM  Link dGlobal
        6  -> liftM4 Cycle (D.list32 Name.decode) (D.list32 (liftM2 (,) Name.decode dExpr)) (D.list32 dDef) (D.set32 dGlobal)
        7  -> liftM  Manager dEffectsType
        8  -> liftM2 Kernel (D.list32 K.dChunk) (D.set32 dGlobal)
        9  -> liftM2 PortIncoming dExpr (D.set32 dGlobal)
        10 -> liftM2 PortOutgoing dExpr (D.set32 dGlobal)
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
