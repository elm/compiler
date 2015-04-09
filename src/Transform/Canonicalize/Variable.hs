{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Variable where

import Control.Monad.Error
import qualified Data.Either as Either
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.EditDistance as Dist

import qualified AST.Helpers as Help
import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Transform.Canonicalize.Environment as Env
import Elm.Utils ((|>))


variable :: Environment -> String -> Canonicalizer String Var.Canonical
variable env var =
  case splitName var of
    Right (name, varName)
        | Module.nameIsNative name ->
            Env.using (Var.Canonical (Var.Module name) varName)

    _ ->
        case Set.toList `fmap` Map.lookup var (_values env) of
          Just [v] -> Env.using v
          Just vs  -> preferLocals env "variable" vs var
          Nothing  -> notFound "variable" (Map.keys (_values env)) var


tvar
    :: Environment
    -> String
    -> Canonicalizer String (Either Var.Canonical (Var.Canonical, [String], Type.CanonicalType))
tvar env var =
  case adts ++ aliases of
    []  -> notFound "type" (Map.keys (_adts env) ++ Map.keys (_aliases env)) var
    [v] -> found extract v
    vs  -> preferLocals' env extract "type" vs var
  where
    adts =
        map Left (maybe [] Set.toList (Map.lookup var (_adts env)))

    aliases =
        map Right (maybe [] Set.toList (Map.lookup var (_aliases env)))

    extract value =
        case value of
          Left v -> v
          Right (v,_,_) -> v


pvar :: Environment -> String -> Canonicalizer String Var.Canonical
pvar env var =
    case Set.toList `fmap` Map.lookup var (_patterns env) of
      Just [v] -> Env.using v
      Just vs  -> preferLocals env "pattern" vs var
      Nothing  -> notFound "pattern" (Map.keys (_patterns env)) var


-- FOUND

found :: (a -> Var.Canonical) -> a -> Canonicalizer String a
found extract v =
  do  _ <- Env.using (extract v)
      return v


preferLocals
    :: Environment
    -> String
    -> [Var.Canonical]
    -> String
    -> Canonicalizer String Var.Canonical
preferLocals env =
  preferLocals' env id


preferLocals'
    :: Environment
    -> (a -> Var.Canonical)
    -> String
    -> [a]
    -> String
    -> Canonicalizer String a
preferLocals' env extract kind possibilities var =
    case filter (isLocal . extract) possibilities of
      []     -> ambiguous possibilities
      [v]    -> found extract v
      locals -> ambiguous locals
    where
      isLocal :: Var.Canonical -> Bool
      isLocal (Var.Canonical home _) =
          case home of
            Var.Local -> True
            Var.BuiltIn -> False
            Var.Module name ->
                name == Env._home env

      ambiguous possibleVars =
          throwError msg
        where
          vars = map (Var.toString . extract) possibleVars
          msg = "Ambiguous usage of " ++ kind ++ " '" ++ var ++ "'.\n" ++
                "    Disambiguate between: " ++ List.intercalate ", " vars


-- NOT FOUND HELPERS

splitName :: String -> Either String ([String], String)
splitName var =
  case Help.splitDots var of
    [x] -> Left x
    xs -> Right (init xs, last xs)


getName :: Either String ([String], String) -> String
getName name =
  case name of
    Left x -> x
    Right (_, x) -> x


nameToString :: ([String], String) -> String
nameToString (modul, name) =
  Module.nameToString (modul ++ [name])


isOp :: Either String ([String], String) -> Bool
isOp name =
  Help.isOp (getName name)


distance :: String -> String -> Int
distance x y =
  Dist.restrictedDamerauLevenshteinDistance Dist.defaultEditCosts x y


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


-- NOT FOUND

notFound :: String -> [String] -> String -> Canonicalizer String a
notFound kind possibilities var =
  let possibleNames =
        map splitName possibilities

      name =
        splitName var

      closeNames =
        possibleNames
          |> filter (\n -> isOp name == isOp n)
          |> nearbyNames getName name

      (exposed, qualified) =
        Either.partitionEithers closeNames

      message =
        case name of
          Left _ ->
              closeExposedMessage exposed ++ closeQualifiedMessage qualified

          Right (modul, _) ->
              qualifiedMessage modul (Either.rights possibleNames) qualified

    in
        throwError $ "Could not find " ++ kind ++ " '" ++ var ++ "'." ++ message



closeExposedMessage :: [String] -> String
closeExposedMessage exposed =
  if null exposed
    then ""
    else
      "\n\nClose matches include:" ++ concatMap ("\n    " ++) exposed


closeQualifiedMessage :: [([String], String)] -> String
closeQualifiedMessage qualified =
  if null qualified
    then ""
    else
      "\n\nMaybe you forgot to say which module it came from?\n"
      ++ "Close qualified names include:"
      ++ concatMap (("\n    " ++) . nameToString) qualified
      ++ usingImportsMessage


qualifiedMessage :: [String] -> [([String], String)] -> [([String], String)] -> String
qualifiedMessage modul allQualified qualified =
  let availableModules =
        Set.fromList (map fst allQualified)
  in
      case Set.member modul availableModules of
        True ->
          let inSameModule =
                filter ((==) modul . fst) qualified
          in
            if null inSameModule
              then ""
              else
                "\n\nClose matches include:"
                ++ concatMap (("\n    " ++) . nameToString) inSameModule

        False ->
          let closeModules =
                Set.toList availableModules
                  |> map Module.nameToString
                  |> nearbyNames id (Module.nameToString modul)
          in
            case closeModules of
              [] ->
                "\n\nLooks like the prefix '" ++ Module.nameToString modul
                ++ "' is not in scope. Is it spelled correctly?"
                ++ "\nIs it imported correctly?"
                ++ usingImportsMessage

              _ ->
                "\n\nClose matches to '" ++ Module.nameToString modul ++ "' include:"
                ++ concatMap ("\n    " ++) closeModules


usingImportsMessage :: String
usingImportsMessage =
  "\n\nYou can read about how imports work at the following address:"
  ++ "\n<http://elm-lang.org/learn/Syntax.elm#modules>"

