{-# OPTIONS_GHC -Wall #-}
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
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Binary (Binary, get, put, getWord8, putWord8)
import qualified Data.Map as Map
import qualified Data.Name as Name
import Data.Name (Name)
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Utils.Shader as Shader
import qualified Data.Index as Index
import qualified Elm.Float as EF
import qualified Elm.Kernel as K
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Elm.String as ES
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Annotation as A



-- EXPRESSIONS


data Expr
  = Bool Bool
  | Chr ES.String
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
  = IndexBuiltin Index.ZeroBased Path
  | IndexCustom Index.ZeroBased Path
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


instance Binary Global where
  get = liftM2 Global get get
  put (Global a b) = put a >> put b


instance Binary Expr where
  put expr =
    case expr of
      Bool a           -> putWord8  0 >> put a
      Chr a            -> putWord8  1 >> put a
      Str a            -> putWord8  2 >> put a
      Int a            -> putWord8  3 >> put a
      Float a          -> putWord8  4 >> put a
      VarLocal a       -> putWord8  5 >> put a
      VarGlobal a      -> putWord8  6 >> put a
      VarEnum a b      -> putWord8  7 >> put a >> put b
      VarBox a         -> putWord8  8 >> put a
      VarCycle a b     -> putWord8  9 >> put a >> put b
      VarDebug a b c d -> putWord8 10 >> put a >> put b >> put c >> put d
      VarKernel a b    -> putWord8 11 >> put a >> put b
      List a           -> putWord8 12 >> put a
      Function a b     -> putWord8 13 >> put a >> put b
      Call a b         -> putWord8 14 >> put a >> put b
      TailCall a b     -> putWord8 15 >> put a >> put b
      If a b           -> putWord8 16 >> put a >> put b
      Let a b          -> putWord8 17 >> put a >> put b
      Destruct a b     -> putWord8 18 >> put a >> put b
      Case a b c d     -> putWord8 19 >> put a >> put b >> put c >> put d
      Accessor a       -> putWord8 20 >> put a
      Access a b       -> putWord8 21 >> put a >> put b
      Update a b       -> putWord8 22 >> put a >> put b
      Record a         -> putWord8 23 >> put a
      Unit             -> putWord8 24
      Tuple a b c      -> putWord8 25 >> put a >> put b >> put c
      Shader a b c     -> putWord8 26 >> put a >> put b >> put c

  get =
    do  word <- getWord8
        case word of
          0  -> liftM  Bool get
          1  -> liftM  Chr get
          2  -> liftM  Str get
          3  -> liftM  Int get
          4  -> liftM  Float get
          5  -> liftM  VarLocal get
          6  -> liftM  VarGlobal get
          7  -> liftM2 VarEnum get get
          8  -> liftM  VarBox get
          9  -> liftM2 VarCycle get get
          10 -> liftM4 VarDebug get get get get
          11 -> liftM2 VarKernel get get
          12 -> liftM  List get
          13 -> liftM2 Function get get
          14 -> liftM2 Call get get
          15 -> liftM2 TailCall get get
          16 -> liftM2 If get get
          17 -> liftM2 Let get get
          18 -> liftM2 Destruct get get
          19 -> liftM4 Case get get get get
          20 -> liftM  Accessor get
          21 -> liftM2 Access get get
          22 -> liftM2 Update get get
          23 -> liftM  Record get
          24 -> pure   Unit
          25 -> liftM3 Tuple get get get
          26 -> liftM3 Shader get get get
          _  -> fail "problem getting Opt.Expr binary"


instance Binary Def where
  put def =
    case def of
      Def a b       -> putWord8 0 >> put a >> put b
      TailDef a b c -> putWord8 1 >> put a >> put b >> put c

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 Def get get
          1 -> liftM3 TailDef get get get
          _ -> fail "problem getting Opt.Def binary"


instance Binary Destructor where
  get = liftM2 Destructor get get
  put (Destructor a b) = put a >> put b


instance Binary Path where
  put destructor =
    case destructor of
      IndexBuiltin a b -> putWord8 0 >> put a >> put b
      IndexCustom a b -> putWord8 1 >> put a >> put b
      Field a b -> putWord8 2 >> put a >> put b
      Unbox a   -> putWord8 3 >> put a
      Root a    -> putWord8 4 >> put a

  get =
    do  word <- getWord8
        case word of
          0 -> liftM2 IndexBuiltin get get
          1 -> liftM2 IndexCustom get get
          2 -> liftM2 Field get get
          3 -> liftM  Unbox get
          4 -> liftM  Root get
          _ -> fail "problem getting Opt.Path binary"


instance (Binary a) => Binary (Decider a) where
  put decider =
    case decider of
      Leaf a       -> putWord8 0 >> put a
      Chain a b c  -> putWord8 1 >> put a >> put b >> put c
      FanOut a b c -> putWord8 2 >> put a >> put b >> put c

  get =
    do  word <- getWord8
        case word of
          0 -> liftM  Leaf get
          1 -> liftM3 Chain get get get
          2 -> liftM3 FanOut get get get
          _ -> fail "problem getting Opt.Decider binary"


instance Binary Choice where
  put choice =
    case choice of
      Inline expr -> putWord8 0 >> put expr
      Jump index  -> putWord8 1 >> put index

  get =
    do  word <- getWord8
        case word of
          0 -> liftM Inline get
          1 -> liftM Jump get
          _ -> fail "problem getting Opt.Choice binary"



instance Binary GlobalGraph where
  get = liftM2 GlobalGraph get get
  put (GlobalGraph a b) = put a >> put b


instance Binary LocalGraph where
  get = liftM3 LocalGraph get get get
  put (LocalGraph a b c) = put a >> put b >> put c


instance Binary Main where
  put main =
    case main of
      Static      -> putWord8 0
      Dynamic a b -> putWord8 1 >> put a >> put b

  get =
    do  word <- getWord8
        case word of
          0 -> return Static
          1 -> liftM2 Dynamic get get
          _ -> fail "problem getting Opt.Main binary"


instance Binary Node where
  put node =
    case node of
      Define a b           -> putWord8  0 >> put a >> put b
      DefineTailFunc a b c -> putWord8  1 >> put a >> put b >> put c
      Ctor a b             -> putWord8  2 >> put a >> put b
      Enum a               -> putWord8  3 >> put a
      Box                  -> putWord8  4
      Link a               -> putWord8  5 >> put a
      Cycle a b c d        -> putWord8  6 >> put a >> put b >> put c >> put d
      Manager a            -> putWord8  7 >> put a
      Kernel a b           -> putWord8  8 >> put a >> put b
      PortIncoming a b     -> putWord8  9 >> put a >> put b
      PortOutgoing a b     -> putWord8 10 >> put a >> put b

  get =
    do  word <- getWord8
        case word of
          0  -> liftM2 Define get get
          1  -> liftM3 DefineTailFunc get get get
          2  -> liftM2 Ctor get get
          3  -> liftM  Enum get
          4  -> return Box
          5  -> liftM  Link get
          6  -> liftM4 Cycle get get get get
          7  -> liftM  Manager get
          8  -> liftM2 Kernel get get
          9  -> liftM2 PortIncoming get get
          10 -> liftM2 PortOutgoing get get
          _  -> fail "problem getting Opt.Node binary"


instance Binary EffectsType where
  put effectsType =
    case effectsType of
      Cmd -> putWord8 0
      Sub -> putWord8 1
      Fx  -> putWord8 2

  get =
    do  word <- getWord8
        case word of
          0 -> return Cmd
          1 -> return Sub
          2 -> return Fx
          _ -> fail "problem getting Opt.EffectsType binary"
