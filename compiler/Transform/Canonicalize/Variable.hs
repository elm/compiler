{-# OPTIONS_GHC -W #-}
module Transform.Canonicalize.Variable where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import AST.Helpers as Help
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Transform.Canonicalize.Environment

variable :: Environment -> String -> Either String Var.Canonical
variable env var =
  case modul of
    Just ms@(m:_)
        | m == "Native" -> return (Var.Canonical home name)
            where
              home = Var.Module (List.intercalate "." ms)

    _ -> case Map.lookup var (_values env) of
           Just [v] -> return v
           Just vs   -> Left (ambiguous "variable" (map Var.toString vs) var)
           Nothing  -> Left (notFound "variable" (Map.keys (_values env)) var)
  where
    (modul, name) = case Help.splitDots var of
                      [x] -> (Nothing, x)
                      xs  -> (Just (init xs), last xs)

tvar :: Environment -> String
     -> Either String (Either Var.Canonical (Var.Canonical, [String], Type.Type Var.Canonical))
tvar env var =
  case adts ++ aliases of
    []  -> Left $ notFound "type" (Map.keys (_adts env) ++ Map.keys (_aliases env)) var
    [v] -> return v
    vs  -> Left $ ambiguous "type" (map toString vs) var
  where
    adts    = map Left  . fromMaybe [] $ Map.lookup var (_adts env)
    aliases = map Right . fromMaybe [] $ Map.lookup var (_aliases env)
    toString v =
        Var.toString $ case v of
                         Left x -> x
                         Right (x,_,_) -> x

pvar :: Environment -> String -> Either String Var.Canonical
pvar env var =
    case Map.lookup var (_patterns env) of
      Just [v] -> return v
      Just vs  -> Left (ambiguous "pattern" (map Var.toString vs) var)
      Nothing  -> Left (notFound "pattern" (Map.keys (_patterns env)) var)

notFound :: String -> [String] -> String -> String
notFound kind possibilities var =
    "Could not find " ++ kind ++ " '" ++ var ++ "'." ++ msg
  where
    matches = filter (List.isInfixOf var) possibilities
    msg = if null matches then "" else
              "\nClose matches include: " ++ List.intercalate ", " matches

ambiguous :: String -> [String] -> String -> String
ambiguous kind possibilities var =
    "Ambiguous usage of " ++ kind ++ " '" ++ var ++ "'.\n" ++
    "    Disambiguate between: " ++ List.intercalate ", " possibilities

