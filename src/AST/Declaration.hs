{-# OPTIONS_GHC -Wall #-}
module AST.Declaration where

import Data.Binary

import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Type as Type
import qualified Reporting.Annotation as A



-- SOURCE DECLARATIONS


type Source =
  CommentOr (A.Located Raw)


data CommentOr a
  = Comment (A.Located String)
  | Whatever a


data Raw
  = Def Source.Def
  | Union (Union Type.Raw)
  | Alias (Alias Type.Raw)
  | Fixity Infix
  | Port String Type.Raw



-- STRUCTURED DECLARATIONS


data Decls def tipe =
  Decls
    { _defs :: [A.Commented def]
    , _unions :: [A.Commented (Union tipe)]
    , _aliases :: [A.Commented (Alias tipe)]
    , _infixes :: [Infix]
    }


type Valid =
  Decls Valid.Def Type.Raw


type Canonical =
  Decls Canonical.Def Type.Canonical


addDef :: A.Commented d -> Decls d t -> Decls d t
addDef def decls =
  decls { _defs = def : _defs decls }


addUnion :: A.Commented (Union t) -> Decls d t -> Decls d t
addUnion union decls =
  decls { _unions = union : _unions decls }


addAlias :: A.Commented (Alias t) -> Decls d t -> Decls d t
addAlias alias decls =
  decls { _aliases = alias : _aliases decls }


addInfix :: Infix -> Decls d t -> Decls d t
addInfix fixity decls =
  decls { _infixes = fixity : _infixes decls }



-- TYPE DECLARATIONS


data Type body =
  Type
    { _name :: String
    , _args :: [String]
    , _body :: body
    }


type Union tipe =
  Type [(String, [tipe])]


type Alias tipe =
  Type tipe



-- INFIX STUFF


data Infix =
  Infix
    { _op :: String
    , _associativity :: Assoc
    , _precedence :: Int
    }


data Assoc = L | N | R
    deriving (Eq)


assocToString :: Assoc -> String
assocToString assoc =
  case assoc of
    L ->
      "left"

    N ->
      "non"

    R ->
      "right"



-- BINARY CONVERSION


instance Binary Infix where
  get =
    Infix <$> get <*> get <*> get

  put (Infix op assoc prec) =
    do  put op
        put assoc
        put prec


instance Binary Assoc where
  get =
    do  n <- getWord8
        return $ case n of
          0 -> L
          1 -> N
          2 -> R
          _ -> error "Error reading valid associativity from serialized string"

  put assoc =
    putWord8 $
      case assoc of
        L -> 0
        N -> 1
        R -> 2
