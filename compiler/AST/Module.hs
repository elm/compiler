{-# OPTIONS_GHC -W #-}
module AST.Module where

import Data.Binary
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Applicative ((<$>),(<*>))

import AST.Declaration (AnnotatedDecl)
import qualified AST.Annotation as A
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Declaration as Decl
import qualified AST.Type as Type
import qualified AST.Variable as Var

import AST.PrettyPrint
import Text.PrettyPrint as P

import qualified Elm.Internal.Version as Version

data Module exs body = Module
    { names   :: [String]
    , path    :: FilePath
    , exports :: exs
    , imports :: [(String, ImportMethod)]
    , comment :: Maybe String
    , body    :: body
    } deriving (Show)

getName :: Module exs body -> String
getName modul =
    List.intercalate "." (names modul)

data CanonicalBody = CanonicalBody
    { program   :: Canonical.Expr
    , types     :: Types
    , defDocs   :: Map.Map String String
    , fixities  :: [AnnotatedDecl (Decl.Assoc, Int, String)]
    , aliases   :: Aliases
    , datatypes :: ADTs
    , ports     :: [AnnotatedDecl String]
    } deriving (Show)

type SourceModule    = Module (Var.Listing Var.Value) [Decl.SourceDecl]
type ValidModule     = Module (Var.Listing Var.Value) [Decl.ValidDecl]
type CanonicalModule = Module [Var.Value] CanonicalBody

type Interfaces = Map.Map String Interface

type Types   = Map.Map String Type.CanonicalType
type Aliases = Map.Map String (AnnotatedDecl ([String], Type.CanonicalType))
type ADTs    = Map.Map String (AnnotatedDecl (AdtInfo String))

type AdtInfo v = ( [String], [(v, [Type.CanonicalType])] )
type CanonicalAdt = (Var.Canonical, AdtInfo Var.Canonical)

data Interface = Interface
    { iVersion  :: Version.Version
    , iTypes    :: Map.Map String Type.CanonicalType
    , iImports  :: [(String, ImportMethod)]
    , iAdts     :: Map.Map String (AdtInfo String)
    , iAliases  :: Map.Map String ([String], Type.CanonicalType)
    , iFixities :: [(Decl.Assoc, Int, String)]
    , iPorts    :: [String]
    } deriving (Show)

toInterface :: CanonicalModule -> Interface
toInterface modul =
    let body' = body modul in
    Interface
    { iVersion  = Version.elmVersion
    , iTypes    = types body'
    , iImports  = imports modul
    , iAdts     = Map.map A.value $ datatypes body'
    , iAliases  = Map.map A.value $ aliases body'
    , iFixities = map A.value $ fixities body'
    , iPorts    = map A.value $ ports body'
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
    deriving (Show)

open :: ImportMethod
open = Open (Var.openListing)

importing :: [Var.Value] -> ImportMethod
importing xs = Open (Var.Listing xs False)

instance Binary ImportMethod where
    put method =
        case method of
          As alias     -> putWord8 0 >> put alias
          Open listing -> putWord8 1 >> put listing

    get = do tag <- getWord8
             case tag of
               0 -> As   <$> get
               1 -> Open <$> get
               _ -> error "Error reading valid ImportMethod type from serialized string"

instance (Pretty exs, Pretty body) => Pretty (Module exs body) where
  pretty (Module names _ exs ims _ body) =
      P.vcat [modul, P.text "", prettyImports, P.text "", pretty body]
    where 
      modul = P.text "module" <+> name <+> pretty exs <+> P.text "where"
      name = P.text (List.intercalate "." names)

      prettyImports = P.vcat $ map prettyMethod ims

prettyMethod :: (String, ImportMethod) -> Doc
prettyMethod (name, method) =
    case method of
      As alias
          | name == alias -> P.empty
          | otherwise     -> P.text "as" <+> P.text alias

      Open listing -> pretty listing
