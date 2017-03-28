{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
  ( Decl(..)
  , Def(..)
  , Expr(..)
  , Decider(..), Choice(..)
  )
  where


import Data.Binary
import qualified Data.Set as Set
import Data.Text (Text)

import qualified AST.Effects as Effects
import qualified AST.Expression.Canonical as Can
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- TOP LEVEL DECLARATIONS


data Decl =
  Decl
    { _deps :: Set.Set Var.Global
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
    | VarGlobal ModuleName.Canonical Text
    | List [Expr]
    | Binop ModuleName.Canonical Text Expr Expr
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
    | OutgoingPort Text Type.Canonical
    | IncomingPort Text Type.Canonical
    | Program Can.Main Expr
    | GLShader Text
    | Crash ModuleName.Canonical R.Region (Maybe Expr)


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
    Decl <$> get <*> get <*> get

  put (Decl deps fx body) =
    put deps >> put fx >> put body


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
          0  -> Literal <$> get
          1  -> VarLocal <$> get
          2  -> VarGlobal <$> get <*> get
          3  -> List <$> get
          4  -> Binop <$> get <*> get <*> get <*> get
          5  -> Function <$> get <*> get
          6  -> Call <$> get <*> get
          7  -> TailCall <$> get <*> get <*> get
          8  -> If <$> get <*> get
          9  -> Let <$> get <*> get
          10 -> Case <$> get <*> get <*> get
          11 -> Ctor <$> get <*> get
          12 -> CtorAccess <$> get <*> get
          13 -> Access <$> get <*> get
          14 -> Update <$> get <*> get
          15 -> Record <$> get
          16 -> Cmd <$> get <*> get
          17 -> Sub <$> get <*> get
          18 -> OutgoingPort <$> get <*> get
          19 -> IncomingPort <$> get <*> get
          20 -> Program <$> get <*> get
          21 -> GLShader <$> get
          22 -> Crash <$> get <*> get <*> get
          _  -> error "problem getting Opt.Expr binary"

  put expr =
    case expr of
      Literal a        -> putWord8  0 >> put a
      VarLocal a       -> putWord8  1 >> put a
      VarGlobal a b    -> putWord8  2 >> put a >> put b
      List a           -> putWord8  3 >> put a
      Binop a b c d    -> putWord8  4 >> put a >> put b >> put c >> put d
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


instance (Binary a) => Binary (Decider a) where
  get =
    do  word <- getWord8
        case word of
          0 -> Leaf <$> get
          1 -> Chain <$> get <*> get <*> get
          2 -> FanOut <$> get <*> get <*> get
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
          0 -> Inline <$> get
          1 -> Jump <$> get
          _ -> error "problem getting Opt.Choice binary"

