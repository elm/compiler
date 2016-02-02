module Elm.Compiler.Module
    ( Interface, Interfaces
    , Name(Name)
    , nameToPath
    , nameToString, nameFromString
    , hyphenate, dehyphenate
    , defaultImports
    , interfacePorts
    , interfaceAliasedTypes
    , CanonicalName, canonicalName
    )
  where

import Control.Monad (mzero)
import qualified Data.Aeson as Json
import Data.Binary
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified Elm.Compiler.Imports as Imports
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Elm.Package as Package


-- INTERFACES

type Interface = Module.Interface


type Interfaces = Module.Interfaces


interfacePorts :: Interface -> [String]
interfacePorts interface =
    Module.iPorts interface


interfaceAliasedTypes :: Interface -> Map.Map String Type.Type
interfaceAliasedTypes interface =
    Map.map Extract.toAliasedType (Module.iTypes interface)


-- NAMES

newtype Name = Name ModuleName.Raw
    deriving (Eq, Ord)


type CanonicalName = ModuleName.Canonical


canonicalName :: Package.Name -> Name -> CanonicalName
canonicalName pkgName (Name name) =
    ModuleName.Canonical pkgName name


defaultImports :: [Name]
defaultImports =
    map (Name . fst) Imports.defaults


-- STRING CONVERSIONS for NAMES

nameToPath :: Name -> FilePath
nameToPath (Name names) =
    List.foldl1 (</>) names


nameToString :: Name -> String
nameToString (Name names) =
    List.intercalate "." names


nameFromString :: String -> Maybe Name
nameFromString =
    fromString '.'


hyphenate :: Name -> String
hyphenate (Name names) =
    List.intercalate "-" names


dehyphenate :: String -> Maybe Name
dehyphenate =
    fromString '-'


fromString :: Char -> String -> Maybe Name
fromString sep raw =
    Name `fmap` mapM isLegit names
  where
    names =
        filter (/= [sep]) (List.groupBy (\a b -> a /= sep && b /= sep) raw)

    isLegit name =
        case name of
            [] -> Nothing
            char:rest ->
                if Char.isUpper char && all legitChar rest
                    then Just name
                    else Nothing

    legitChar char =
        Char.isAlphaNum char || char `elem` "_'"


-- JSON for NAME

instance Json.ToJSON Name where
    toJSON name =
        Json.toJSON (nameToString name)


instance Json.FromJSON Name where
    parseJSON (Json.String text) =
        let rawName = Text.unpack text in
        case nameFromString rawName of
            Nothing -> fail (rawName ++ " is not a valid module name")
            Just name -> return name

    parseJSON _ = mzero


-- BINARY for NAME

instance Binary Name where
  get =
    fmap Name get

  put (Name names) =
    put names
