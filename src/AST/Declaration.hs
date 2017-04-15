{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Declaration where

import Prelude hiding (Either(..))
import Control.Monad (liftM3)
import Data.Binary
import Data.Text (Text)

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.Source as Src
import qualified AST.Type as Type
import qualified Reporting.Annotation as A



-- SOURCE DECLARATIONS


type Source =
  CommentOr (A.Located Raw)


data CommentOr a
  = Comment (A.Located Text)
  | Whatever a


data Raw
  = Def Src.RawDef
  | Union (Union Type.Raw)
  | Alias (Alias Type.Raw)
  | Fixity Infix
  | Port Text Type.Raw



-- STRUCTURED DECLARATIONS


data Decls def tipe =
  Decls
    { _defs :: [A.Commented def]
    , _unions :: [A.Commented (Union tipe)]
    , _aliases :: [A.Commented (Alias tipe)]
    , _infixes :: [Infix]
    }


type Valid =
  Decls Src.ValidDef Type.Raw


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
    { _name :: Text
    , _args :: [Text]
    , _body :: body
    }


type Union tipe =
  Type [(Text, [tipe])]


type Alias tipe =
  Type tipe



-- INFIX STUFF


data Infix =
  Infix
    { _op :: Text
    , _associativity :: Assoc
    , _precedence :: Int
    }


data Assoc = Left | Non | Right
    deriving (Eq)


defaultAssociativity :: Assoc
defaultAssociativity =
  Left


defaultPrecedence :: Int
defaultPrecedence =
  9


assocToText :: Assoc -> Text
assocToText assoc =
  case assoc of
    Left ->
      "left"

    Non ->
      "non"

    Right ->
      "right"



-- BINARY


instance Binary Infix where
  get =
    liftM3 Infix get get get

  put (Infix op assoc prec) =
    do  put op
        put assoc
        put prec


instance Binary Assoc where
  get =
    do  n <- getWord8
        return $
          case n of
            0 -> Left
            1 -> Non
            2 -> Right
            _ -> error "Error reading valid associativity from serialized string"

  put assoc =
    putWord8 $
      case assoc of
        Left  -> 0
        Non   -> 1
        Right -> 2