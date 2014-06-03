{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Variable where

import Control.Monad.Error
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import AST.Helpers as Help
import qualified AST.Type as Type
import qualified AST.Variable as Var
import Transform.Canonicalize.Environment as Env

variable :: Environment -> String -> Canonicalizer String Var.Canonical
variable env var =
  case modul of
    Just ms@(m:_)
        | m == "Native" -> Env.using (Var.Canonical home name)
            where
              home = Var.Module (List.intercalate "." ms)

    _ -> case Map.lookup var (_values env) of
           Just [v] -> Env.using v
           Just vs  -> Env.using =<< preferLocals "variable" vs var
           Nothing  -> notFound "variable" (Map.keys (_values env)) var
  where
    (modul, name) = case Help.splitDots var of
                      [x] -> (Nothing, x)
                      xs  -> (Just (init xs), last xs)

tvar :: Environment -> String
     -> Canonicalizer String (Either Var.Canonical (Var.Canonical, [String], Type.CanonicalType))
tvar env var =
  case adts ++ aliases of
    []  -> notFound "type" (Map.keys (_adts env) ++ Map.keys (_aliases env)) var
    [v] -> return v
    vs  -> preferLocals' extract "type" vs var
  where
    adts    = map Left .  fromMaybe [] $ Map.lookup var (_adts env)
    aliases = map Right . fromMaybe [] $ Map.lookup var (_aliases env)

    extract value =
        case value of
          Left v -> v
          Right (v,_,_) -> v

pvar :: Environment -> String -> Canonicalizer String Var.Canonical
pvar env var =
    case Map.lookup var (_patterns env) of
      Just [v] -> Env.using v
      Just vs  -> Env.using =<< preferLocals "pattern" vs var
      Nothing  -> notFound "pattern" (Map.keys (_patterns env)) var

notFound :: String -> [String] -> String -> Canonicalizer String a
notFound kind possibilities var =
    throwError $ "Could not find " ++ kind ++ " '" ++ var ++ "'." ++ msg
  where
    matches = filter (List.isInfixOf var) possibilities
    msg = if null matches then "" else
              "\nClose matches include: " ++ List.intercalate ", " matches

preferLocals :: String -> [Var.Canonical] -> String -> Canonicalizer String Var.Canonical
preferLocals = preferLocals' id

preferLocals' :: (a -> Var.Canonical) -> String -> [a] -> String -> Canonicalizer String a
preferLocals' extract kind possibilities var =
    case filter (isLocal . extract) possibilities of
      []     -> ambiguous possibilities
      [v]    -> return v
      locals -> ambiguous locals
    where
      isLocal :: Var.Canonical -> Bool
      isLocal (Var.Canonical Var.Local _) = True
      isLocal (Var.Canonical _         _) = False

      ambiguous possibleVars =
          throwError msg
        where
          vars = map (Var.toString . extract) possibleVars
          msg = "Ambiguous usage of " ++ kind ++ " '" ++ var ++ "'.\n" ++
                "    Disambiguate between: " ++ List.intercalate ", " vars

