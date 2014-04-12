{-# OPTIONS_GHC -W #-}
module AST.Module where

import Data.Binary
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))

import qualified AST.Expression.Canonical as Canonical
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified SourceSyntax.Declaration as Decl

import qualified Elm.Internal.Version as Version

data Module exs body = Module
    { names   :: [String]
    , path    :: FilePath
    , exports :: exs
    , imports :: [(String, ImportMethod)]
    , body    :: body
    }

data CanonicalBody = CanonicalBody
    { program   :: Canonical.Expr
    , types     :: Types
    , fixities  :: [(Decl.Assoc, Int, String)]
    , aliases   :: [Alias]
    , datatypes :: [ADT]
    , ports     :: [String]
    }

type SourceModule    = Module (Var.Listing Var.Value) [Decl.SourceDecl]
type ValidModule     = Module (Var.Listing Var.Value) [Decl.ValidDecl]
type CanonicalModule = Module [Var.Value] CanonicalBody

type Interfaces = Map.Map String Interface

type Types = Map.Map String (Type.Type Var.Canonical)
type ADT = ( String, [String], [(String, [Type.Type Var.Canonical])] )
type Alias = (String, [String], Type.Type Var.Canonical )

data Interface = Interface
    { iVersion  :: Version.Version
    , iTypes    :: Types
    , iImports  :: [(String, ImportMethod)]
    , iAdts     :: [ADT]
    , iAliases  :: [Alias]
    , iFixities :: [(Decl.Assoc, Int, String)]
    , iPorts    :: [String]
    }

toInterface :: CanonicalModule -> Interface
toInterface modul =
    let body' = body modul in
    Interface
    { iVersion  = Version.elmVersion
    , iTypes    = types body'
    , iImports  = imports modul
    , iAdts     = datatypes body'
    , iAliases  = aliases body'
    , iFixities = fixities body'
    , iPorts    = ports body'
    }

instance Binary Interface where
  get = Interface <$> get <*> get <*> get <*> get <*> get <*> get <*> get
  put modul = do
      put (iVersion modul)
      put (iTypes modul)
      put (iImports modul)
      put (iAdts modul)
      put (iAliases modul)
      put (iFixities modul)
      put (iPorts modul)

data ImportMethod
    = As !String
    | Open !(Var.Listing Var.Value)

open :: ImportMethod
open = Open (Var.openListing)

importing :: [String] -> ImportMethod
importing xs = Open (Var.Listing (map Var.Value xs) False)

instance Binary ImportMethod where
    put method =
        let put' n info = putWord8 n >> put info in
        case method of
          As alias     -> put' 0 alias
          Open listing -> put' 1 listing

    get = do tag <- getWord8
             case tag of
               0 -> As   <$> get
               1 -> Open <$> get
               _ -> error "Error reading valid ImportMethod type from serialized string"

{-
data Module port def = Module
    { name    :: [String]
    , exports :: Var.Listing Var.Value
    , imports :: [(String, ImportMethod)]
    , decls   :: [Declaration' port def]
    }

instance (Pretty port, Pretty def) => Pretty (Module port def) where
  pretty (Module names exs ims decls) =
      P.vcat [modul, P.text "", prettyImports, P.text "", prettyDecls]
    where 
      modul = P.text "module" <+> name <+> prettyExports <+> P.text "where"
      name = P.text (List.intercalate "." names)

      prettyExports
          | Var._open exs = P.empty
          | otherwise         = pretty exs

      prettyImports = P.vcat $ map prettyMethod ims

      prettyDecls = P.sep $ map pretty decls

prettyMethod :: (String, ImportMethod) -> Doc
prettyMethod (name, method) =
    case method of
      As alias
          | name == alias -> P.empty
          | otherwise     -> P.text "as" <+> P.text alias

      Open listing -> pretty listing

-}
