module Elm.Compiler.Module
    ( Interface, Name(Name)
    , nameToPath
    , nameToString, nameFromString
    , hyphenate, dehyphenate
    , defaultImports
    , interfacePorts
    , interfaceTypes
    )
  where

import Control.Monad (mzero)
import qualified Data.Aeson as Json
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified AST.Module as Module
import qualified Transform.AddDefaultImports as Defaults
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Type.Extract as Extract


-- EXPOSED TYPES

type Interface = Module.Interface


newtype Name = Name [String]
    deriving (Eq, Ord)


defaultImports :: [Name]
defaultImports =
    map Name (Map.keys Defaults.defaultImports)


-- POKING AROUND INTERFACES

interfacePorts :: Interface -> [String]
interfacePorts interface =
    Module.iPorts interface


interfaceTypes :: Interface -> Map.Map String Type.Type
interfaceTypes interface =
    Map.map Extract.fromInternalType (Module.iTypes interface)


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
