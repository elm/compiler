{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AST.Module
    ( Header(..), HeaderDecl(..), defaultHeaderDecl
    , Module(..)

    , Source, SourceInfo(..), SourceTag(..), SourceSettings, emptySettings
    , Valid, ValidInfo(..)
    , Canonical, Optimized, Info(..)

    , UserImport, DefaultImport, ImportMethod(..)

    , Types
    , Aliases
    , Unions, UnionInfo, CanonicalUnion

    , Interfaces, Interface(..), toInterface, privatize
    )
    where

import Data.Binary
import qualified Data.Map as Map
import Data.Text (Text)

import qualified AST.Declaration as Decl
import qualified AST.Effects as Effects
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Expression.Optimized as Optimized
import qualified AST.Module.Name as Name
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Docs.AST as Docs
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- HEADERS FOR PARSING


{-| Basic info needed to identify modules and determine dependencies. -}
data Header imports =
  Header
    { _decl :: Maybe HeaderDecl
    , _imports :: imports
    }


data HeaderDecl =
  HeaderDecl
    { _tag :: SourceTag
    , _name :: Name.Raw
    , _exports :: Var.Listing (A.Located Var.Value)
    , _settings :: SourceSettings
    , _docs :: A.Located (Maybe Text)
    }


defaultHeaderDecl :: HeaderDecl
defaultHeaderDecl =
  let
    zero = R.Position 1 1
    noDocs = A.at zero zero Nothing
  in
    HeaderDecl Normal "Main" Var.openListing emptySettings noDocs



-- MODULES


data Module phase =
  Module
    { name :: Name.Canonical
    , info :: phase
    }


type Source =
  Module SourceInfo


data SourceInfo =
  Source
    { srcTag :: SourceTag
    , srcSettings :: SourceSettings
    , srcDocs :: A.Located (Maybe Text)
    , srcExports :: Var.Listing (A.Located Var.Value)
    , srcImports :: [UserImport]
    , srcDecls :: [Decl.Source]
    }


data SourceTag
  = Normal
  | Effect R.Region
  | Port R.Region


type SourceSettings =
  A.Located [(A.Located Text, A.Located Text)]


emptySettings :: SourceSettings
emptySettings =
  A.A (error "region of empty settings should not be needed") []


type Valid =
  Module ValidInfo


data ValidInfo =
  Valid
    { validDocs :: A.Located (Maybe Text)
    , validExports :: Var.Listing (A.Located Var.Value)
    , validImports :: ([DefaultImport], [UserImport])
    , validDecls :: Decl.Valid
    , validEffects :: Effects.Raw
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
    { alias :: Maybe Text
    , exposedVars :: !(Var.Listing Var.Value)
    }



-- LATE PHASE MODULE INFORMATION


data Info program =
  Info
    { docs :: A.Located (Maybe Docs.Centralized)
    , exports :: [Var.Value]
    , imports :: [Name.Raw]
    , program :: program
    , types :: Types
    , fixities :: [Decl.Infix]
    , aliases :: Aliases
    , unions :: Unions
    , effects :: Effects.Canonical
    }


type Types =
  Map.Map Text Type.Canonical


type Aliases =
  Map.Map Text ([Text], Type.Canonical)


type Unions =
  Map.Map Text (UnionInfo Text)


type UnionInfo v =
  ( [Text], [(v, [Type.Canonical])] )


type CanonicalUnion =
  ( Var.Canonical, UnionInfo Var.Canonical )



-- INTERFACES


type Interfaces =
  Map.Map Name.Canonical Interface


{-| Key facts about a module, used when reading info from .elmi files. -}
data Interface =
  Interface
    { iExports  :: [Var.Value]
    , iImports  :: [Name.Raw] -- TODO perhaps use this to crawl faster
    , iTypes    :: Types
    , iUnions   :: Unions
    , iAliases  :: Aliases
    , iFixities :: [Decl.Infix]
    }


toInterface :: Optimized -> Interface
toInterface modul =
  let
    myInfo =
      info modul
  in
    Interface
      { iExports  = exports myInfo
      , iImports  = imports myInfo
      , iTypes    = types myInfo
      , iUnions   = unions myInfo
      , iAliases  = aliases myInfo
      , iFixities = fixities myInfo
      }


privatize :: Interface -> Maybe Interface
privatize (Interface _ _ _ myUnions myAliases _) =
  if Map.null myUnions && Map.null myAliases then
    Nothing

  else
    Just $ Interface
      { iExports  = []
      , iImports  = []
      , iTypes    = Map.empty
      , iUnions   = myUnions
      , iAliases  = myAliases
      , iFixities = []
      }


instance Binary Interface where
  get =
    Interface <$> get <*> get <*> get <*> get <*> get <*> get

  put modul =
    do  put (iExports modul)
        put (iImports modul)
        put (iTypes modul)
        put (iUnions modul)
        put (iAliases modul)
        put (iFixities modul)
