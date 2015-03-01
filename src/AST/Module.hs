module AST.Module
    ( Interfaces
    , Types, Aliases, ADTs
    , AdtInfo, CanonicalAdt
    , SourceModule, ValidModule, CanonicalModule
    , Module(..), CanonicalBody(..)
    , HeaderAndImports(..)
    , Name, nameToString, nameIsNative
    , Interface(..), toInterface
    , ImportMethod(..)
    ) where

import Data.Binary
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Declaration as Decl
import qualified AST.Type as Type
import qualified AST.Variable as Var
import AST.PrettyPrint
import qualified Elm.Compiler.Version as Compiler
import Text.PrettyPrint as P


-- HELPFUL TYPE ALIASES

type Interfaces = Map.Map Name Interface

type Types   = Map.Map String Type.CanonicalType
type Aliases = Map.Map String ([String], Type.CanonicalType)
type ADTs    = Map.Map String (AdtInfo String)

type AdtInfo v = ( [String], [(v, [Type.CanonicalType])] )
type CanonicalAdt = (Var.Canonical, AdtInfo Var.Canonical)


-- MODULES

type SourceModule =
    Module (Var.Listing Var.Value) [Decl.SourceDecl]

type ValidModule =
    Module (Var.Listing Var.Value) [Decl.ValidDecl]

type CanonicalModule =
    Module [Var.Value] CanonicalBody


data Module exports body = Module
    { names   :: Name
    , path    :: FilePath
    , exports :: exports
    , imports :: [(Name, ImportMethod)]
    , body    :: body
    }

data CanonicalBody = CanonicalBody
    { program   :: Canonical.Expr
    , types     :: Types
    , fixities  :: [(Decl.Assoc, Int, String)]
    , aliases   :: Aliases
    , datatypes :: ADTs
    , ports     :: [String]
    }


-- HEADERS

{-| Basic info needed to identify modules and determine dependencies. -}
data HeaderAndImports = HeaderAndImports
    { _names :: Name
    , _exports :: Var.Listing Var.Value
    , _imports :: [(Name, ImportMethod)]
    }


type Name = [String] -- must be non-empty


nameToString :: Name -> String
nameToString =
  List.intercalate "."


nameIsNative :: Name -> Bool
nameIsNative name =
  case name of
    "Native" : _ -> True
    _ -> False



-- INTERFACES

{-| Key facts about a module, used when reading info from .elmi files. -}
data Interface = Interface
    { iVersion  :: String
    , iExports  :: [Var.Value]
    , iTypes    :: Types
    , iImports  :: [(Name, ImportMethod)]
    , iAdts     :: ADTs
    , iAliases  :: Aliases
    , iFixities :: [(Decl.Assoc, Int, String)]
    , iPorts    :: [String]
    }

toInterface :: CanonicalModule -> Interface
toInterface modul =
    let body' = body modul in
    Interface
    { iVersion  = Compiler.version
    , iExports  = exports modul
    , iTypes    = types body'
    , iImports  = imports modul
    , iAdts     = datatypes body'
    , iAliases  = aliases body'
    , iFixities = fixities body'
    , iPorts    = ports body'
    }

instance Binary Interface where
  get = Interface <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  put modul = do
      put (iVersion modul)
      put (iExports modul)
      put (iTypes modul)
      put (iImports modul)
      put (iAdts modul)
      put (iAliases modul)
      put (iFixities modul)
      put (iPorts modul)


-- IMPORT METHOD

data ImportMethod = ImportMethod
    { alias :: Maybe String
    , exposedVars :: !(Var.Listing Var.Value)
    }


instance Binary ImportMethod where
    put (ImportMethod alias exposedVars) =
      do  put alias
          put exposedVars

    get =
      ImportMethod <$> get <*> get


-- PRETTY PRINTING

instance (Pretty exs, Pretty body) => Pretty (Module exs body) where
  pretty (Module names _ exs ims body) =
      P.vcat [modul, P.text "", prettyImports, P.text "", pretty body]
    where
      modul =
        P.text "module" <+> name <+> pretty exs <+> P.text "where"

      name =
        P.text (nameToString names)

      prettyImports =
        P.vcat $ map prettyMethod ims


prettyMethod :: (Name, ImportMethod) -> Doc
prettyMethod (name, ImportMethod maybeAlias exposedVars) =
  let prettyAlias =
        case maybeAlias of
          Nothing -> P.empty
          Just alias ->
            P.text "as" <+> P.text alias

      prettyExposed =
        if exposedVars == Var.closedListing
          then P.empty
          else P.text "exposing" <+> pretty exposedVars
  in
      P.text "import"
          <+> P.text (nameToString name)
          <+> prettyAlias
          <+> prettyExposed
