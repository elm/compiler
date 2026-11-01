module AST.Source
  ( Expr, Expr_(..)
  , VarType(..), Branch(..)
  , Def(..)
  , Pattern, Pattern_(..)
  , Type, Type_(..)
  --
  , Module(..)
  , Import(..)
  , Decls(..)
  , Decl(..)
  , Value(..)
  , Alias(..)
  , Union(..)
  , Variant(..)
  , Port(..)
  , Infix(..)
  --
  , Header(..)
  , Style(..)
  , Manager(..)
  --
  , Docs(..)
  , Comment(..)
  --
  , Exposing(..)
  , Exposed(..)
  , Privacy(..)
  --
  , getName
  , getImportName
  )
  where


import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.Shader as Shader
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
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
  | Lambda Pattern [Pattern] Expr
  | Call Expr Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [A.Located Def] Expr
  | Case Expr Branch [Branch]
  | Empty
  | Record (A.Located N.Name, Expr) [(A.Located N.Name, Expr)]
  | Update (A.Located N.Name) (A.Located N.Name, Expr) [(A.Located N.Name, Expr)]
  | Access Expr (A.Located N.Name)
  | Accessor N.Name
  | Unit
  | Tuple Expr Expr [Expr]
  | Shader Shader.Source Shader.Types


data VarType = LowVar | CapVar


data Branch =
  Branch Pattern Expr



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
  = TLambda Type Type [Type]
  | TVar T.Var
  | TType A.Region T.Name [Type]
  | TTypeQual A.Region Module.Prefix T.Name [Type]
  | TUnit
  | TTuple Type Type [Type]
  | TEmpty
  | TRecord (A.Located N.Name, Type) [(A.Located N.Name, Type)]
  | TExtRec (A.Located N.Name, Type) [(A.Located N.Name, Type)] (A.Located T.Var)



-- MODULE


data Module =
  Module
    { _header  :: Header
    , _docs    :: Docs
    , _imports :: [Import]
    , _decls   :: Decls
    , _infixes :: [A.Located Infix]
    }


data Import =
  Import
    { _import :: A.Located Module.Name
    , _alias :: Maybe Module.Prefix
    , _exposing :: Exposing
    }


data Decls
  = NoDecls A.Region
  | RevDecls Decl [Decl]


data Decl
  = Value_ (Maybe Comment) (A.Located Value)
  | Union_ (Maybe Comment) (A.Located Union)
  | Alias_ (Maybe Comment) (A.Located Alias)
  | Port_  (Maybe Comment) (A.Located Port)


data Value = Value (A.Located N.Name) [Pattern] Expr (Maybe Type)
data Alias = Alias (A.Located T.Name) [A.Located T.Var] Type
data Union = Union (A.Located T.Name) [A.Located T.Var] Variant [Variant]
data Variant = Variant (A.Located N.Name) [Type]
data Port = Port (A.Located N.Name) Type

data Infix = Infix (A.Located Op.Name) Op.Associativity Op.Precedence N.Name



-- HEADER


data Header
  = YesHeader Style (A.Located Module.Name) (A.Located Exposing)
  | NoHeader A.Region


data Style
  = Normal A.Region
  | Ports A.Region
  | Effect A.Region Manager


data Manager
  = Cmd (A.Located T.Name)
  | Sub (A.Located T.Name)
  | Fx  (A.Located T.Name) (A.Located T.Name)



-- DOCS


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



-- HELPERS


getName :: Header -> Module.Name
getName header =
  case header of
    YesHeader _ (A.At _ name) _ -> name
    NoHeader  _                 -> Module.main


getImportName :: Import -> Module.Name
getImportName (Import (A.At _ name) _ _) =
  name
