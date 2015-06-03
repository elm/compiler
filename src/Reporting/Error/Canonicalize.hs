{-# OPTIONS_GHC -Wall #-}
module Reporting.Error.Canonicalize where

import Data.Function (on)
import qualified Data.List as List
import qualified Text.EditDistance as Dist
import qualified Text.PrettyPrint as P

import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Elm.Utils ((|>))
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as Region
import qualified Reporting.Report as Report


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
    | MutuallyRecursive [(Region.Region, String, [String], Type.Raw)]


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


-- TO REPORT

namingError :: String -> String -> Report.Report
namingError pre post =
  Report.simple "NAMING ERROR" pre post


toReport :: P.Dealiaser -> Error -> Report.Report
toReport dealiaser err =
  case err of
    Var (VarError kind name problem suggestions) ->
        let var = kind ++ " `" ++ name ++ "`"
        in
        case problem of
          Ambiguous ->
              namingError
                ("This usage of " ++ var ++ " is ambiguous.")
                (maybeYouWant suggestions)

          UnknownQualifier qualifier ->
              namingError
                ("Cannot find " ++ var ++ ", qualifier `" ++ qualifier ++ "` is not in scope.")
                (maybeYouWant suggestions)

          QualifiedUnknown qualifier ->
              namingError
                ("Cannot find " ++ var ++ ", module `" ++ qualifier ++ "` does not expose that value.")
                (maybeYouWant suggestions)

          ExposedUnknown ->
              namingError
                ("Cannot find " ++ var)
                (maybeYouWant suggestions)

    Pattern patternError ->
        case patternError of
          PatternArgMismatch var expected actual ->
              argMismatchReport "Pattern" var expected actual

    Alias aliasError ->
        case aliasError of
          ArgMismatch var expected actual ->
              argMismatchReport "Type" var expected actual

          SelfRecursive name tvars tipe ->
              Report.simple "ALIAS PROBLEM" "This type alias is recursive, forming an infinite type!" $
                "Type aliases are just names for more fundamental types, so I go through\n"
                ++ "and expand them all to know the real underlying type. The problem is that\n"
                ++ "when I expand a recursive type alias, it keeps getting bigger and bigger.\n"
                ++ "Dealiasing results in an infinitely large type! Try this instead:\n\n"
                ++ "    type " ++ name ++ concatMap (' ':) tvars ++ " ="
                ++ P.render (P.nest 8 (P.hang (P.text name) 2 (P.pretty dealiaser True tipe)))
                ++ "\n\nThis creates a brand new type that cannot be expanded. It is similar types\n"
                ++ "like List which are recursive, but do not get expanded into infinite types."

          MutuallyRecursive aliases ->
              Report.simple "ALIAS PROBLEM" "This type alias is part of a mutually recursive set of type aliases." $
                "The following type aliases all depend on each other in some way:\n"
                ++ concatMap showName aliases ++ "\n\n"
                ++ "The problem is that when you try to expand any of these type aliases, they\n"
                ++ "expand infinitely! To fix, first try to centralize them into one alias."
            where
              showName (Region.Region start _, name, _, _) =
                  "\n    " ++ name ++ replicate (65 - length name) ' '
                  ++ "line " ++ show (Region.line start)

    Import name importError ->
        let moduleName = Module.nameToString name
        in
        case importError of
          ModuleNotFound suggestions ->
              namingError
                ("Could not find a module named `" ++ moduleName ++ "`")
                (maybeYouWant (map Module.nameToString suggestions))

          ValueNotFound value suggestions ->
              namingError
                ("Module `" ++ moduleName ++ "` does not expose `" ++ value ++ "`")
                (maybeYouWant suggestions)

    Export name suggestions ->
        namingError
          ("Could not export `" ++ name ++ "` which is not defined in this module.")
          (maybeYouWant suggestions)

    Port (PortError name isInbound _rootType localType maybeMessage) ->
        let
          boundPort =
            if isInbound then "inbound port" else "outbound port"

          preHint =
            "Trying to send " ++ maybe "an unsupported type" id maybeMessage
            ++ " through " ++ boundPort ++ " `" ++ name ++ "`"

          postHint =
            "The specific unsupported type is:\n\n"
            ++ P.render (P.nest 4 (P.pretty dealiaser False localType)) ++ "\n\n"
            ++ "The types of values that can flow through " ++ boundPort ++ "s include:\n"
            ++ "Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, Tuples,\n"
            ++ "Json.Values, and concrete records."
        in
          Report.simple "PORT ERROR" preHint postHint


argMismatchReport :: String -> Var.Canonical -> Int -> Int -> Report.Report
argMismatchReport kind var expected actual =
  let
    preHint =
      kind ++ " " ++ Var.toString var ++ " has too "
      ++ (if actual < expected then "few" else "many")
      ++ " arguments."

    postHint =
      "Expecting " ++ show expected ++ ", but got " ++ show actual ++ "."
  in
      Report.simple "ARGUMENT" preHint postHint


maybeYouWant :: [String] -> String
maybeYouWant suggestions =
  case suggestions of
    [] ->
        ""

    _:_ ->
        "Maybe you want one of the following?\n"
        ++ concatMap ("\n    "++) suggestions


extractSuggestions :: Error -> Maybe [String]
extractSuggestions err =
  case err of
    Var (VarError _ _ _ suggestions) ->
        Just suggestions

    Pattern _ ->
        Nothing

    Alias _ ->
        Nothing

    Import _ importError ->
        case importError of
          ModuleNotFound suggestions ->
              Just (map Module.nameToString suggestions)

          ValueNotFound _ suggestions ->
              Just suggestions

    Export _ suggestions ->
        Just suggestions

    Port _ ->
        Nothing
