module AST.Module
    ( Header(..), Module(..), Tag(..)

    , Source, SourceInfo(..)
    , Valid, ValidInfo(..)
    , Canonical, Optimized, Info(..)

    , UserImport, DefaultImport, ImportMethod(..)

    , Effects(..)
    , Types
    , Aliases
    , Unions, UnionInfo, CanonicalUnion

    , Interfaces, Interface(..), toInterface
    )
    where

import Data.Binary
import qualified Data.Map as Map

import qualified AST.Declaration as Decl
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.Optimized as Optimized
import qualified AST.Expression.Valid as Valid
import qualified AST.Module.Name as Name
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Docs.AST as Docs
import qualified Elm.Package as Package
import qualified Elm.Compiler.Version as Compiler
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- HEADERS FOR PARSING


{-| Basic info needed to identify modules and determine dependencies. -}
data Header imports =
  Header
    { _tag :: Tag
    , _name :: Name.Raw
    , _docs :: A.Located (Maybe String)
    , _exports :: Var.Listing (A.Located Var.Value)
    , _imports :: imports
    }



-- MODULES


data Module phase =
  Module
    { tag :: Tag
    , name :: Name.Canonical
    , path :: FilePath
    , info :: phase
    }


data Tag
  = None
  | Effect R.Region
  | Foreign R.Region


type Source =
  Module SourceInfo


data SourceInfo =
  Source
    { srcDocs :: A.Located (Maybe String)
    , srcExports :: Var.Listing (A.Located Var.Value)
    , srcImports :: [UserImport]
    , srcDecls :: [Decl.Source]
    }


type Valid =
  Module ValidInfo


data ValidInfo =
  Valid
    { validDocs :: A.Located (Maybe String)
    , validExports :: Var.Listing (A.Located Var.Value)
    , validImports :: ([DefaultImport], [UserImport])
    , validDecls :: [Decl.Valid]
    , validEffects :: Valid.Effects
    }


type Canonical =
  Module (Info Canonical.Expr)


type Optimized =
  Module (Info [Optimized.Def])



-- IMPORTS


type UserImport = A.Located (Name.Raw, ImportMethod)


type DefaultImport = (Name.Raw, ImportMethod)


data ImportMethod =
  ImportMethod
    { alias :: Maybe String
    , exposedVars :: !(Var.Listing Var.Value)
    }



-- LATE PHASE MODULE INFORMATION


data Info expr =
  Info
    { docs :: A.Located (Maybe Docs.Centralized)
    , exports :: [Var.Value]
    , imports :: [Name.Raw]
    , program :: expr
    , types :: Types
    , fixities :: [(Decl.Assoc, Int, String)]
    , aliases :: Aliases
    , unions :: Unions
    , effects :: Effects
    }


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


data Effects =
  Effects
    { cmds :: [String]
    , subs :: [String]
    }



-- INTERFACES


type Interfaces =
  Map.Map Name.Canonical Interface


{-| Key facts about a module, used when reading info from .elmi files. -}
data Interface =
  Interface
    { iVersion  :: Package.Version
    , iPackage  :: Package.Name
    , iExports  :: [Var.Value]
    , iImports  :: [Name.Raw]
    , iTypes    :: Types
    , iUnions   :: Unions
    , iAliases  :: Aliases
    , iFixities :: [(Decl.Assoc, Int, String)]
    }


toInterface :: Package.Name -> Optimized -> Interface
toInterface pkgName modul =
  let
    myInfo =
      info modul
  in
    Interface
      { iVersion  = Compiler.version
      , iPackage  = pkgName
      , iExports  = exports myInfo
      , iImports  = imports myInfo
      , iTypes    = types myInfo
      , iUnions   = unions myInfo
      , iAliases  = aliases myInfo
      , iFixities = fixities myInfo
      }


instance Binary Interface where
  get =
    Interface <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

  put modul =
    do  put (iVersion modul)
        put (iPackage modul)
        put (iExports modul)
        put (iImports modul)
        put (iTypes modul)
        put (iUnions modul)
        put (iAliases modul)
        put (iFixities modul)
