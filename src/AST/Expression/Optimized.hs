{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
  ( Decl(..)
  , Def(..)
  , Expr(..)
  , Main(..)
  , Decider(..), Choice(..)
  )
  where


import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Data.Binary
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Effects as Effects
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- TOP LEVEL DECLARATIONS


data Decl =
  Decl
    { _direct :: Set.Set Var.Global
    , _indirect :: Set.Set Var.Global
    , _effects :: Maybe Effects.ManagerType
    , _body :: !Def
    }



-- DEFINITIONS


data Def
  = Def Expr
  | TailDef [Text] Expr



-- EXPRESSIONS


data Expr
    = Literal Literal.Literal
    | VarLocal Text
    | VarGlobal Var.Global
    | List [Expr]
    | Binop Var.Global Expr Expr
    | Function [Text] Expr
    | Call Expr [Expr]
    | TailCall Text [Text] [Expr]
    | If [(Expr, Expr)] Expr
    | Let [(Text, Def)] Expr
    | Case Text (Decider Choice) [(Int, Expr)]
    | Ctor Text [Expr]
    | CtorAccess Expr Int
    | Access Expr Text
    | Update Expr [(Text, Expr)]
    | Record [(Text, Expr)]
    | Cmd ModuleName.Canonical Effects.ManagerType
    | Sub ModuleName.Canonical Effects.ManagerType
    | OutgoingPort Text Expr
    | IncomingPort Text Expr
    | Program Main Expr
    | GLShader Text
    | Crash ModuleName.Canonical R.Region (Maybe Expr)


data Main
  = Static
  | Dynamic Expr


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



-- BINARY


instance Binary Decl where
  get =
    liftM4 Decl get get get get

  put (Decl direct indirect fx body) =
    put direct >> put indirect >> put fx >> put body


instance Binary Def where
  get =
    do  word <- getWord8
        case word of
          0 -> Def <$> get
          1 -> TailDef <$> get <*> get
          _ -> error "problem getting Opt.Def binary"

  put def =
    case def of
      Def body ->
        putWord8 0 >> put body

      TailDef args body ->
        putWord8 1 >> put args >> put body


instance Binary Expr where
  get =
    do  word <- getWord8
        case word of
          0  -> liftM  Literal get
          1  -> liftM  VarLocal get
          2  -> liftM  VarGlobal get
          3  -> liftM  List get
          4  -> liftM3 Binop get get get
          5  -> liftM2 Function get get
          6  -> liftM2 Call get get
          7  -> liftM3 TailCall get get get
          8  -> liftM2 If get get
          9  -> liftM2 Let get get
          10 -> liftM3 Case get get get
          11 -> liftM2 Ctor get get
          12 -> liftM2 CtorAccess get get
          13 -> liftM2 Access get get
          14 -> liftM2 Update get get
          15 -> liftM  Record get
          16 -> liftM2 Cmd get get
          17 -> liftM2 Sub get get
          18 -> liftM2 OutgoingPort get get
          19 -> liftM2 IncomingPort get get
          20 -> liftM2 Program get get
          21 -> liftM  GLShader get
          22 -> liftM3 Crash get get get
          _  -> error "problem getting Opt.Expr binary"

  put expr =
    case expr of
      Literal a        -> putWord8  0 >> put a
      VarLocal a       -> putWord8  1 >> put a
      VarGlobal a      -> putWord8  2 >> put a
      List a           -> putWord8  3 >> put a
      Binop a b c      -> putWord8  4 >> put a >> put b >> put c
      Function a b     -> putWord8  5 >> put a >> put b
      Call a b         -> putWord8  6 >> put a >> put b
      TailCall a b c   -> putWord8  7 >> put a >> put b >> put c
      If a b           -> putWord8  8 >> put a >> put b
      Let a b          -> putWord8  9 >> put a >> put b
      Case a b c       -> putWord8 10 >> put a >> put b >> put c
      Ctor a b         -> putWord8 11 >> put a >> put b
      CtorAccess a b   -> putWord8 12 >> put a >> put b
      Access a b       -> putWord8 13 >> put a >> put b
      Update a b       -> putWord8 14 >> put a >> put b
      Record a         -> putWord8 15 >> put a
      Cmd a b          -> putWord8 16 >> put a >> put b
      Sub a b          -> putWord8 17 >> put a >> put b
      OutgoingPort a b -> putWord8 18 >> put a >> put b
      IncomingPort a b -> putWord8 19 >> put a >> put b
      Program a b      -> putWord8 20 >> put a >> put b
      GLShader a       -> putWord8 21 >> put a
      Crash a b c      -> putWord8 22 >> put a >> put b >> put c


instance Binary Main where
  put main =
    case main of
      Static       -> putWord8 0
      Dynamic expr -> putWord8 1 >> put expr

  get =
    do  word <- getWord8
        case word of
          0 -> pure Static
          1 -> liftM Dynamic get
          _ -> error "problem getting Opt.Main binary"


instance (Binary a) => Binary (Decider a) where
  get =
    do  word <- getWord8
        case word of
          0 -> liftM  Leaf get
          1 -> liftM3 Chain get get get
          2 -> liftM3 FanOut get get get
          _ -> error "problem getting Opt.Decider binary"

  put decider =
    case decider of
      Leaf a ->
        do  putWord8 0
            put a

      Chain testChain success failure ->
        do  putWord8 1
            put testChain
            put success
            put failure

      FanOut path tests fallback ->
        do  putWord8 2
            put path
            put tests
            put fallback


instance Binary Choice where
  put choice =
    case choice of
      Inline expr -> putWord8 0 >> put expr
      Jump index -> putWord8 1 >> put index

  get =
    do  word <- getWord8
        case word of
          0 -> liftM Inline get
          1 -> liftM Jump get
          _ -> error "problem getting Opt.Choice binary"

