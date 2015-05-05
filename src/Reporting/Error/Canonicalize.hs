{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Reporting.Error.Canonicalize where

--import Data.Aeson ((.=))
import qualified Data.Aeson as Json
--import qualified Data.Aeson.Types as Json
import Data.Function (on)
import qualified Data.List as List
import qualified Text.EditDistance as Dist
import qualified Text.PrettyPrint as P

import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


data Error
    = Var VarError
    | Pattern PatternError
    | Alias AliasError
    | Import Module.Name ImportError
    | Export String [String]
    | Port PortError


-- VARIABLES

data VarError = VarError
    { varKind :: String
    , varName :: String
    , varProblem :: VarProblem
    , varSuggestions :: [String]
    }


data VarProblem
    = Ambiguous
    | UnknownQualifier String
    | QualifiedUnknown String
    | ExposedUnknown


variable :: String -> String -> VarProblem -> [String] -> Error
variable kind name problem suggestions =
  Var (VarError kind name problem suggestions)


-- PATTERN

data PatternError
    = PatternArgMismatch Var.Canonical Int Int


argMismatch :: Var.Canonical -> Int -> Int -> Error
argMismatch name expected actual =
  Pattern (PatternArgMismatch name expected actual)


-- IMPORTS

data ImportError
    = ModuleNotFound [Module.Name]
    | ValueNotFound String [String]


moduleNotFound :: Module.Name -> [Module.Name] -> Error
moduleNotFound name possibilities =
  Import name (ModuleNotFound possibilities)


valueNotFound :: Module.Name -> String -> [String] -> Error
valueNotFound name value possibilities =
  Import name (ValueNotFound value possibilities)


-- ALIASES

data AliasError
    = ArgMismatch Var.Canonical Int Int
    | SelfRecursive String [String] Type.Raw
    | MutuallyRecursive [(R.Region, String, [String], Type.Raw)]


alias :: Var.Canonical -> Int -> Int -> Error
alias name expected actual =
  Alias (ArgMismatch name expected actual)


-- PORTS

data PortError = PortError
    { portName :: String
    , portIsInbound :: Bool
    , portRootType :: Type.Canonical
    , portLocalType :: Type.Canonical
    , portMessage :: Maybe String
    }


port
    :: String
    -> Bool
    -> Type.Canonical
    -> Type.Canonical
    -> Maybe String
    -> Error
port name isInbound rootType localType maybeMessage =
  Port (PortError name isInbound rootType localType maybeMessage)


-- NEARBY NAMES

nearbyNames :: (a -> String) -> a -> [a] -> [a]
nearbyNames format name names =
  let editDistance =
        if length (format name) < 3 then 1 else 2
  in
      names
        |> map (\x -> (distance (format name) (format x), x))
        |> List.sortBy (compare `on` fst)
        |> filter ( (<= editDistance) . abs . fst )
        |> map snd


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y


-- TO STRING

toString :: R.Region -> Error -> String
toString region err =
  case err of
    _ -> error "Canonicalize.toString" region


-- TO JSON

toJson :: Error -> Json.Value
toJson err =
  case err of
    _ -> error "Canonicalize.toJson"


-- HINTS

toHint :: Error -> String
toHint err =
  case err of
    Var (VarError kind name problem suggestions) ->
        let var = kind ++ "'" ++ name ++ "'"
        in
        case problem of
          Ambiguous ->
              "This usage of " ++ var ++ " is ambiguous"
              ++ maybeYouWant "\n\n" suggestions

          UnknownQualifier qualifier ->
              "Cannot find " ++ var ++ "\n\n"
              ++ "The qualifier " ++ qualifier ++ " is not in scope."
              ++ maybeYouWant "\n" suggestions

          QualifiedUnknown qualifier ->
              "Cannot find " ++ var ++ "\n\n"
              ++ "The module called " ++ qualifier ++ " does not expose that value."
              ++ maybeYouWant "\n" suggestions

          ExposedUnknown ->
              "Cannot find " ++ var
              ++ maybeYouWant "\n\n" suggestions

    Pattern patternError ->
        case patternError of
          PatternArgMismatch var expected actual ->
              argMismatchHint "pattern" var expected actual

    Alias aliasError ->
        case aliasError of
          ArgMismatch var expected actual ->
              argMismatchHint "type" var expected actual

          SelfRecursive name tvars tipe ->
              "This type alias is recursive, forming an infinite type!\n\n"
              ++ "Type aliases are just names for more fundamental types, so I go through\n"
              ++ "and expand them all to know the real underlying type. The problem is that\n"
              ++ "when I expand a recursive type alias, it keeps getting bigger and bigger.\n"
              ++ "Dealiasing results in an infinitely large type! Try this instead:\n\n"
              ++ "    type " ++ name ++ concatMap (' ':) tvars ++ " ="
              ++ P.render (P.nest 8 (P.hang (P.text name) 2 (P.pretty True tipe)))
              ++ "\n\nThis creates a brand new type that cannot be expanded. It is similar types\n"
              ++ "like List which are recursive, but do not get expanded into infinite types."

          MutuallyRecursive aliases ->
              "This type alias is part of a mutually recursive set of type aliases.\n"
              ++ "The following type aliases all depend on each other in some way:\n"
              ++ concatMap showName aliases ++ "\n\n"
              ++ "The problem is that when you try to expand any of these type aliases, they\n"
              ++ "expand infinitely! To fix, first try to centralize them into one alias."
            where
              showName (R.Region start _, name, _, _) =
                  "\n    " ++ name ++ replicate (65 - length name) ' '
                  ++ "line " ++ show (R.line start)

    Import name importError ->
        case importError of
          ModuleNotFound suggestions ->
              "Could not find a module named '" ++ Module.nameToString name ++ "'"
              ++ maybeYouWant "\n\n" (map Module.nameToString suggestions)

          ValueNotFound value suggestions ->
              "Module '" ++ Module.nameToString name ++ "' does not expose '" ++ value ++ "'"
              ++ maybeYouWant "\n\n" suggestions

    Export name suggestions ->
        "Could not export '" ++ name ++ "'\n\n"
        ++ "It is not defined in this module."
        ++ maybeYouWant " " suggestions

    Port (PortError name isInbound _rootType localType maybeMessage) ->
        "Cannot pass " ++ message ++ " through " ++ boundPort ++ " '" ++ name ++ "':\n\n"
        ++ P.render (P.nest 4 (P.pretty False localType)) ++ "\n\n"
        ++ "The kinds of values that can flow through " ++ boundPort ++ "s include:\n"
        ++ "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, Tuples,\n"
        ++ "Json.Values, and concrete records."
      where
        boundPort =
          if isInbound then "inbound port" else "outbound port"

        message =
          maybe "the following type" id maybeMessage


argMismatchHint :: String -> Var.Canonical -> Int -> Int -> String
argMismatchHint kind var expected actual =
    "The " ++ kind ++ " " ++ Var.toString var ++ " has too "
    ++ (if actual < expected then "few" else "many")
    ++ "arguments.\n\n"
    ++ "Expecting " ++ show expected ++ ", but got " ++ show actual


maybeYouWant :: String -> [String] -> String
maybeYouWant prefix suggestions =
  case suggestions of
    [] ->
        ""

    _:_ ->
        prefix
        ++ "Maybe you want one of the following?\n"
        ++ concatMap ("\n    "++) suggestions

