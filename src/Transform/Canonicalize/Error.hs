{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Error where

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import Data.Function (on)
import qualified Data.List as List
import qualified Text.EditDistance as Dist

import qualified AST.Module as Module
import qualified AST.Type as T
import qualified AST.Variable as Var
import Elm.Utils ((|>))


data Error
    = Var VarError
    | Alias AliasError
    | Import Module.Name ImportError
    | Export String [String]
    | Port PortError


-- VARIABLES

data VarError = VarError
    { varKind :: String
    , varName :: String
    , varProblem :: VarProblem
    }


data VarProblem
    = Ambiguous [String]
    | UnknownQualifier [Module.Name]
    | QualifiedUnknown [String]
    | ExposedUnknown { closeExposed :: [String], closeQualified :: [String] }


var :: String -> String -> VarProblem -> Error
var kind name problem =
  Var (VarError kind name problem)


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
    = ArgMissmatch Var.Canonical Int Int
    | Recursive [(String, [String], T.RawType)]


alias :: Var.Canonical -> Int -> Int -> Error
alias name expected actual =
  Alias (ArgMissmatch name expected actual)


recursiveAlias :: [(String, [String], T.RawType)] -> Error
recursiveAlias aliases =
  Alias (Recursive aliases)


-- PORTS

data PortError = PortError
    { portName :: String
    , portIsInbound :: Bool
    , portRootType :: T.CanonicalType
    , portLocalType :: T.CanonicalType
    , portMessage :: String
    }


port
    :: String
    -> Bool
    -> T.CanonicalType
    -> T.CanonicalType
    -> String
    -> Error
port name isInbound rootType localType message =
  Port (PortError name isInbound rootType localType message)


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


-- JSON

errorToJson err =
    Json.object (generals ++ specifics)
  where
    generals =
        [ "category" .= "canonicalization"
        , "region" .= region
        ]

    specifics =
      case err of
        Var (VarError kind name varProblem) ->
            [ "kind" .= kind
            , "name" .= name
            ]
            ++ varProblemToJson varProblem

        Alias aliasError ->
            case aliasError of
              ArgMissmatch name expected actual ->
                  [ "tag" .= "alias-args"
                  , "name" .= name
                  , "expected-args" .= expected
                  , "actual-args" .= actual
                  ]

              Recursive [(name, tvars, tipe)] ->
                  [ "tag" .= "recursive-alias"
                  ]

              Recursive aliases ->
                  [ "tag" .= "mutually-recursive-aliases"
                  ]

        Import moduleName importError ->
            case importError of
              ModuleNotFound suggestions ->
                  [ "tag" .= "unknown-import"
                  , "name" .= moduleName
                  , "suggestions" .= suggestions
                  ]
              ValueNotFound name suggestions ->
                  []

        Export name suggestions ->
            []

        Port portError ->
            []


varProblemToJson :: VarProblem -> Json.Value
varProblemToJson varProblem =
  let details tag suggestions =
        [ "tag" .= tag
        , "suggestions" .= suggestions
        ]
  in
  case varProblem of
    Ambiguous possibilities ->
        details "ambiguous" possibilities

    UnknownQualifier possibleModules ->
        details "unknown-qualifier" possibleModules

    QualifiedUnknown possibilities ->
        details "qualified-unknown" possibilities

    ExposedUnknown closeExposed closeQualified ->
        details "exposed-unknown" (closeExposed ++ closeQualified)


{-- TO STRING

mutuallyRecursiveMessage :: String
mutuallyRecursiveMessage =
  "The following type aliases are mutually recursive, forming an \
  \infinite type. When you expand them, they just keep getting bigger:"


typeAliasErrorSegue :: String
typeAliasErrorSegue =
  "Try this instead:"


typeAliasErrorExplanation :: String
typeAliasErrorExplanation =
  "It looks very similar, but the 'type' keyword creates a brand new type, \
  \not just an alias for an existing one. This lets us avoid infinitely \
  \expanding it during type inference."


typeAlias
    :: (String, [String], Type.Type var)
    -> D.Declaration' pk def var expr
typeAlias (n,ts,t) =
    D.TypeAlias n ts t


datatype
    :: (String, [String], Type.Type var)
    -> D.Declaration' pk def var expr
datatype (n,ts,t) =
    D.Datatype n ts [(n,[t])]


indented :: [D.ValidDecl] -> Doc
indented decls =
    P.vcat (map prty decls) <> P.text "\n"
  where
    prty decl = P.text "\n    " <> pretty decl

--}
