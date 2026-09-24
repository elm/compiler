module AST.Source
  ( Expr, Expr_(..), VarType(..)
  , Def(..)
  , Pattern, Pattern_(..)
  , Type, Type_(..)
  , Module(..)
  , getName
  , getImportName
  , Import(..)
  , Value(..)
  , Union(..)
  , Alias(..)
  , Infix(..)
  , Port(..)
  , Effects(..)
  , Manager(..)
  , Docs(..)
  , Comment(..)
  , Exposing(..)
  , Exposed(..)
  , Privacy(..)
  )
  where


import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified AST.Utils.Shader as Shader
import qualified Elm.Float as EF
import qualified Elm.String as ES
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A



-- EXPRESSIONS


type Expr = A.Located Expr_


data Expr_
  = Chr Char
  | Str ES.String
  | Int Integer
  | Float EF.Float
  | Var VarType N.Name
  | VarQual VarType Module.Prefix N.Name
  | List [Expr]
  | Op Op.Name
  | Negate Expr
  | Binops [(Expr, A.Located Op.Name)] Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Accessor N.Name
  | Access Expr (A.Located N.Name)
  | Update (A.Located N.Name) [(A.Located N.Name, Expr)]
  | Record [(A.Located N.Name, Expr)]
  | Unit
  | Tuple Expr Expr [Expr]
  | Shader Shader.Source Shader.Types


data VarType = LowVar | CapVar



-- DEFINITIONS


data Def
  = Define (A.Located N.Name) [Pattern] Expr (Maybe Type)
  | Destruct Pattern Expr



-- PATTERN


type Pattern = A.Located Pattern_


data Pattern_
  = PAnything
  | PVar N.Name
  | PRecord [A.Located N.Name]
  | PAlias Pattern (A.Located N.Name)
  | PUnit
  | PTuple Pattern Pattern [Pattern]
  | PCtor A.Region N.Name [Pattern]
  | PCtorQual A.Region Module.Prefix N.Name [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PChr Char
  | PStr ES.String
  | PInt Integer



-- TYPE


type Type =
    A.Located Type_


data Type_
  = TLambda Type Type
  | TVar T.Var
  | TType A.Region T.Name [Type]
  | TTypeQual A.Region Module.Prefix T.Name [Type]
  | TRecord [(A.Located N.Name, Type)] (Maybe (A.Located T.Var))
  | TUnit
  | TTuple Type Type [Type]



-- MODULE


data Module =
  Module
    { _name    :: Maybe (A.Located Module.Name)
    , _exports :: A.Located Exposing
    , _docs    :: Docs
    , _imports :: [Import]
    , _values  :: [A.Located Value]
    , _unions  :: [A.Located Union]
    , _aliases :: [A.Located Alias]
    , _binops  :: [A.Located Infix]
    , _effects :: Effects
    }


getName :: Module -> Module.Name
getName (Module maybeName _ _ _ _ _ _ _ _) =
  case maybeName of
    Just (A.At _ name) -> name
    Nothing            -> Module.main


getImportName :: Import -> Module.Name
getImportName (Import (A.At _ name) _ _) =
  name


data Import =
  Import
    { _import :: A.Located Module.Name
    , _alias :: Maybe Module.Prefix
    , _exposing :: Exposing
    }


data Value = Value (A.Located N.Name) [Pattern] Expr (Maybe Type)
data Union = Union (A.Located T.Name) [A.Located T.Var] [(A.Located N.Name, [Type])]
data Alias = Alias (A.Located T.Name) [A.Located T.Var] Type
data Infix = Infix Op.Name Op.Associativity Op.Precedence N.Name
data Port  = Port (A.Located N.Name) Type


data Effects
  = NoEffects
  | Ports [Port]
  | Manager A.Region Manager


data Manager
  = Cmd (A.Located T.Name)
  | Sub (A.Located T.Name)
  | Fx  (A.Located T.Name) (A.Located T.Name)


data Docs
  = NoDocs A.Region
  | YesDocs Comment [(N.Name, Comment)] [(T.Name, Comment)]


newtype Comment =
  Comment P.Snippet



-- EXPOSING


data Exposing
  = Open
  | Explicit [Exposed]


data Exposed
  = Lower (A.Located N.Name)
  | Upper (A.Located T.Name) Privacy
  | Operator A.Region Op.Name


data Privacy
  = Public A.Region
  | Private
