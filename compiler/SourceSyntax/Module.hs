{-# OPTIONS_GHC -W #-}
module SourceSyntax.Module where

import Data.Binary
import qualified Data.List as List
import qualified Data.Map as Map
import Control.Applicative ((<$>), (<*>))
import Text.PrettyPrint as P

import SourceSyntax.Expression (Expr)
import SourceSyntax.Declaration
import SourceSyntax.PrettyPrint
import SourceSyntax.Type

import qualified Elm.Internal.Version as Version

data Module def =
    Module [String] Exports Imports [def]
    deriving (Show)

type Exports = [String]

type Imports = [(String, ImportMethod)]
data ImportMethod = As String | Importing [String] | Hiding [String]
                    deriving (Eq, Ord, Show)

instance (Pretty def) => Pretty (Module def) where
  pretty (Module modNames exports imports decls) =
      P.vcat [modul, P.text "", prettyImports, P.text "", prettyDecls]
    where 
      prettyDecls = P.sep $ map pretty decls

      modul = P.text "module" <+> moduleName <+> prettyExports <+> P.text "where"
      moduleName = P.text $ List.intercalate "." modNames
      prettyExports =
          case exports of
            [] -> P.empty
            _ -> P.parens . commaCat $ map P.text exports

      prettyImports = P.vcat $ map prettyImport imports
        
      prettyImport (name, method) =
          P.text "import" <+>
          case method of
            As alias ->
                P.text $ name ++ (if name == alias then "" else " as " ++ alias)

            Importing values ->
                P.text name <+> P.parens (commaCat (map P.text values))

            Hiding [] -> P.text ("open " ++ name)
            Hiding _ -> error "invalid import declaration"
                    
instance Binary ImportMethod where
    put method =
        let put' n info = putWord8 n >> put info in
        case method of
          As s         -> put' 0 s
          Importing ss -> put' 1 ss
          Hiding ss    -> put' 2 ss

    get = do tag <- getWord8
             case tag of
               0 -> As        <$> get
               1 -> Importing <$> get
               2 -> Hiding    <$> get
               _ -> error "Error reading valid ImportMethod type from serialized string"

data MetadataModule =
    MetadataModule
    { names     :: [String]
    , path      :: FilePath
    , exports   :: [String]
    , imports   :: [(String, ImportMethod)]
    , program   :: Expr
    , types     :: Map.Map String Type
    , fixities  :: [(Assoc, Int, String)]
    , aliases   :: [Alias]
    , datatypes :: [ADT]
    , ports     :: [String]
    } deriving Show

type Interfaces = Map.Map String ModuleInterface
type ADT = (String, [String], [(String,[Type])])
type Alias = (String, [String], Type)

data ModuleInterface =
    ModuleInterface
    { iVersion  :: Version.Version
    , iTypes    :: Map.Map String Type
    , iImports  :: [(String, ImportMethod)]
    , iAdts     :: [ADT]
    , iAliases  :: [Alias]
    , iFixities :: [(Assoc, Int, String)]
    , iPorts    :: [String]
    } deriving Show

metaToInterface :: MetadataModule -> ModuleInterface
metaToInterface metaModule =
    ModuleInterface
    { iVersion  = Version.elmVersion
    , iTypes    = types metaModule
    , iImports  = imports metaModule
    , iAdts     = datatypes metaModule
    , iAliases  = aliases metaModule
    , iFixities = fixities metaModule
    , iPorts = ports metaModule
    }

instance Binary ModuleInterface where
  get = ModuleInterface <$> get <*> get <*> get <*> get <*> get <*> get <*> get
  put modul = do
      put (iVersion modul)
      put (iTypes modul)
      put (iImports modul)
      put (iAdts modul)
      put (iAliases modul)
      put (iFixities modul)
      put (iPorts modul)
