module AST.Module
    ( Interfaces
    , Types, Aliases, Unions
    , UnionInfo, CanonicalUnion
    , SourceModule, ValidModule, CanonicalModule, Optimized
    , Module(..), Kind(..), Body(..)
    , Header(..)
    , Interface(..), toInterface
    , UserImport, DefaultImport, ImportMethod(..)
    ) where

import Data.Binary
import qualified Data.Map as Map

import qualified AST.Declaration as Decl
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.Optimized as Optimized
import qualified AST.Module.Name as Name
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Docs.AST as Docs
import qualified Elm.Package as Package
import qualified Elm.Compiler.Version as Compiler
import qualified Reporting.Annotation as A



-- HELPFUL TYPE ALIASES


type Interfaces =
  Map.Map Name.Canonical Interface


type Types =
  Map.Map String Type.Canonical


type Aliases =
  Map.Map String ([String], Type.Canonical)


type Unions =
  Map.Map String (UnionInfo String)


type UnionInfo v =
  ( [String], [(v, [Type.Canonical])] )


type CanonicalUnion =
  ( Var.Canonical, UnionInfo Var.Canonical )



-- MODULES


type SourceModule =
    Module
      String
      [UserImport]
      (Var.Listing (A.Located Var.Value))
      [Decl.SourceDecl]


type ValidModule =
    Module
      String
      ([DefaultImport], [UserImport])
      (Var.Listing (A.Located Var.Value))
      [Decl.ValidDecl]


type CanonicalModule =
    Module Docs.Centralized [Name.Raw] [Var.Value] (Body Canonical.Expr)


type Optimized =
    Module Docs.Centralized [Name.Raw] [Var.Value] (Body [Optimized.Def])


data Module docs imports exports body =
  Module
    { kind :: Kind
    , name :: Name.Canonical
    , path :: FilePath
    , docs :: A.Located (Maybe docs)
    , exports :: exports
    , imports :: imports
    , body :: body
    }


data Kind
  = Normal
  | Effect
  | Foreign


data Body expr =
  Body
    { program :: expr
    , types :: Types
    , fixities :: [(Decl.Assoc, Int, String)]
    , aliases :: Aliases
    , unions :: Unions
    }



-- HEADERS


{-| Basic info needed to identify modules and determine dependencies. -}
data Header imports =
  Header
    { _kind :: Kind
    , _name :: Name.Raw
    , _docs :: A.Located (Maybe String)
    , _exports :: Var.Listing (A.Located Var.Value)
    , _imports :: imports
    }



-- IMPORTs


type UserImport = A.Located (Name.Raw, ImportMethod)


type DefaultImport = (Name.Raw, ImportMethod)


data ImportMethod =
  ImportMethod
    { alias :: Maybe String
    , exposedVars :: !(Var.Listing Var.Value)
    }



-- INTERFACES


{-| Key facts about a module, used when reading info from .elmi files. -}
data Interface =
  Interface
    { iVersion  :: Package.Version
    , iPackage  :: Package.Name
    , iExports  :: [Var.Value]
    , iTypes    :: Types
    , iImports  :: [Name.Raw]
    , iUnions   :: Unions
    , iAliases  :: Aliases
    , iFixities :: [(Decl.Assoc, Int, String)]
    }


toInterface :: Package.Name -> Optimized -> Interface
toInterface pkgName modul =
  let
    body' =
      body modul
  in
    Interface
      { iVersion  = Compiler.version
      , iPackage  = pkgName
      , iExports  = exports modul
      , iTypes    = types body'
      , iImports  = imports modul
      , iUnions   = unions body'
      , iAliases  = aliases body'
      , iFixities = fixities body'
      }


instance Binary Interface where
  get =
    Interface <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

  put modul =
    do  put (iVersion modul)
        put (iPackage modul)
        put (iExports modul)
        put (iTypes modul)
        put (iImports modul)
        put (iUnions modul)
        put (iAliases modul)
        put (iFixities modul)
