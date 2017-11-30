{-# OPTIONS_GHC -Wall #-}
module AST.Optimized
  ( Def(..)
  , Expr(..)
  , Global(..)
  , Decider(..)
  , Choice(..)
  , Graph(..)
  , Node(..)
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Binary
import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- EXPRESSIONS


data Expr
  = Chr Text
  | Str Text
  | Int Int
  | Float Double
  | VarLocal N.Name
  | VarGlobal Global
  | VarDebug N.Name ModuleName.Canonical R.Region (Maybe Expr)
  | VarKernel N.Name N.Name
  | List [Expr]
  | Function [N.Name] Expr
  | Call Expr [Expr]
  | TailCall N.Name [N.Name] [Expr]
  | If [(Expr, Expr)] Expr
  | Let Def Expr
  | Case N.Name (Decider Choice) [(Int, Expr)]
  | Ctor N.Name [Expr]
  | CtorAccess Expr Index.ZeroBased
  | Accessor N.Name
  | Access Expr N.Name
  | Update Expr (Map.Map N.Name Expr)
  | Record (Map.Map N.Name Expr)
  | Unit
  | Tuple Expr Expr (Maybe Expr)
  | Shader Text


data Global = Global ModuleName.Canonical N.Name
  deriving (Eq, Ord)



-- DEFINITIONS


data Def
  = Def N.Name Expr
  | TailDef N.Name [N.Name] Expr



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


data Graph =
  Graph
    { _nodes :: Map.Map Global Node
    }


data Node = Node



-- BINARY


instance Binary Global where
  get = liftM2 Global get get
  put (Global a b) = put a >> put b


instance Binary Expr where
  put expr =
    case expr of
      Chr a            -> putWord8  0 >> put a
      Str a            -> putWord8  1 >> put a
      Int a            -> putWord8  2 >> put a
      Float a          -> putWord8  3 >> put a
      VarLocal a       -> putWord8  4 >> put a
      VarGlobal a      -> putWord8  5 >> put a
      VarDebug a b c d -> putWord8  6 >> put a >> put b >> put c >> put d
      VarKernel a b    -> putWord8  7 >> put a >> put b
      List a           -> putWord8  8 >> put a
      Function a b     -> putWord8  9 >> put a >> put b
      Call a b         -> putWord8 10 >> put a >> put b
      TailCall a b c   -> putWord8 11 >> put a >> put b >> put c
      If a b           -> putWord8 12 >> put a >> put b
      Let a b          -> putWord8 13 >> put a >> put b
      Case a b c       -> putWord8 14 >> put a >> put b >> put c
      Ctor a b         -> putWord8 15 >> put a >> put b
      CtorAccess a b   -> putWord8 16 >> put a >> put b
      Accessor a       -> putWord8 17 >> put a
      Access a b       -> putWord8 18 >> put a >> put b
      Update a b       -> putWord8 19 >> put a >> put b
      Record a         -> putWord8 20 >> put a
      Unit             -> putWord8 21
      Tuple a b c      -> putWord8 22 >> put a >> put b >> put c
      Shader a         -> putWord8 23 >> put a

  get =
    do  word <- getWord8
        case word of
          0  -> liftM  Chr get
          1  -> liftM  Str get
          2  -> liftM  Int get
          3  -> liftM  Float get
          4  -> liftM  VarLocal get
          5  -> liftM  VarGlobal get
          6  -> liftM4 VarDebug get get get get
          7  -> liftM2 VarKernel get get
          8  -> liftM  List get
          9  -> liftM2 Function get get
          10 -> liftM2 Call get get
          11 -> liftM3 TailCall get get get
          12 -> liftM2 If get get
          13 -> liftM2 Let get get
          14 -> liftM3 Case get get get
          15 -> liftM2 Ctor get get
          16 -> liftM2 CtorAccess get get
          17 -> liftM  Accessor get
          18 -> liftM2 Access get get
          19 -> liftM2 Update get get
          20 -> liftM  Record get
          21 -> pure   Unit
          22 -> liftM3 Tuple get get get
          23 -> liftM  Shader get
          _  -> error "problem getting Opt.Expr binary"


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
          _ -> error "problem getting Opt.Def binary"


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
          _ -> error "problem getting Opt.Decider binary"


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
          _ -> error "problem getting Opt.Choice binary"
