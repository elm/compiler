module Elm.Compiler.Module
    ( Interface(Interface), Name(Name)
    , nameToPath, nameToString, nameFromString
    )
  where

import Control.Monad (mzero)
import qualified Data.Aeson as Json
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import System.FilePath ((</>))
import qualified AST.Module as Module


newtype Interface = Interface Module.Interface

newtype Name = Name [String]
    deriving (Eq, Ord)


nameToPath :: Name -> FilePath
nameToPath (Name names) =
    List.foldl1 (</>) names


nameToString :: Name -> String
nameToString (Name names) =
    List.intercalate "." names


nameFromString :: String -> Maybe Name
nameFromString raw =
    Name `fmap` mapM isLegit names
  where
    names =
        filter (/= ".") (List.groupBy (\a b -> a /= '.' && b /= '.') raw)

    isLegit name =
        case name of
            [] -> Nothing
            char:rest ->
                if Char.isUpper char && all legitChar rest
                    then Just name
                    else Nothing

    legitChar char =
        Char.isAlphaNum char || char `elem` "_'"


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