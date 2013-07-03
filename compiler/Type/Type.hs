module Type.Type where

import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF
import System.IO.Unsafe

data Term1 a
    = App1 a a
    | Fun1 a a
    | Var1 a
    | EmptyRecord1
    | Record1 (Map.Map String [a]) a
    deriving Show

data TermN a
    = VarN a
    | TermN (Term1 (TermN a))

record fs rec = TermN (Record1 fs rec)

type SchemeName = String
type TypeName = String

data Constraint a b
    = CTrue
    | CEqual a a
    | CAnd [Constraint a b]
    | CLet [Scheme a b] (Constraint a b)
    | CInstance SchemeName a
    deriving Show

data Scheme a b = Scheme {
    rigidQuantifiers :: [b],
    flexibleQuantifiers :: [b],
    constraint :: Constraint a b,
    header :: Map.Map String a    -- mapping from names to types
} deriving Show

data Descriptor = Descriptor {
    structure :: Maybe (Term1 Variable),
    rank :: Int,
    flex :: Flex,
    name :: Maybe TypeName
}

noRank = 0
data Flex = Rigid | Flexible | Constant
     deriving Show

type Variable = UF.Point Descriptor

type Type = TermN Variable
type TypeConstraint = Constraint Type Variable
type TypeScheme = Scheme Type Variable

infixl 8 /\

(/\) :: Constraint a b -> Constraint a b -> Constraint a b
a /\ b = CAnd [a,b]

(===) :: Type -> Type -> TypeConstraint
(===) = CEqual

(<?) :: SchemeName -> Type -> TypeConstraint
x <? t = CInstance x t

infixr 9 ==>
(==>) :: Type -> Type -> Type
a ==> b = TermN (Fun1 a b)

namedVar name = UF.fresh $ Descriptor {
    structure = Nothing,
    rank = noRank,
    flex = Constant,
    name = Just name
  }

flexibleVar = UF.fresh $ Descriptor {
    structure = Nothing,
    rank = noRank,
    flex = Flexible,
    name = Nothing
  }

-- ex qs constraint == exists qs. constraint
ex :: [Variable] -> TypeConstraint -> TypeConstraint
ex fqs constraint = CLet [Scheme [] fqs constraint Map.empty] CTrue

-- fl qs constraint == forall qs. constraint
fl :: [Variable] -> TypeConstraint -> TypeConstraint
fl rqs constraint = CLet [Scheme rqs [] constraint Map.empty] CTrue

exists :: (Type -> IO TypeConstraint) -> IO TypeConstraint
exists f = do
  v <- flexibleVar
  ex [v] <$> f (VarN v)

instance Show a => Show (UF.Point a) where
  show point = unsafePerformIO $ fmap show (UF.descriptor point)

instance Show Descriptor where
  show desc = case name desc of
                Just n -> n
                Nothing -> show (structure desc)

instance Show a => Show (TermN a) where
  show term = case term of
                VarN v -> show v
                TermN t -> "(" ++ show t ++ ")"