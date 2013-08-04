module Transform.Check (mistakes) where

import Transform.SortDefinitions (boundVars)
import SourceSyntax.Everything
import qualified SourceSyntax.Type as T
import Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Data
import Data.Generics.Uniplate.Data
import Text.PrettyPrint as P


mistakes :: (Data t, Data v) => [Declaration t v] -> [Doc]
mistakes decls =
    map P.text $ concatMap findErrors (getLets decls)
  where
    findErrors defs = duplicates defs ++ badOrder defs


getLets :: (Data t, Data v) => [Declaration t v] -> [[Def t v]]
getLets decls = defs : concatMap getSubLets defs
    where
      defs = concatMap (\d -> case d of Definition d -> [d] ; _ -> []) decls

      getSubLets def =
          case def of
            Def pattern expr -> [ defs | Let defs _ <- universeBi expr ]
            TypeAnnotation _ _ -> []


duplicates :: [Def t v] -> [String]
duplicates defs =
    map defMsg (dups definitions) ++ map annMsg (dups annotations)
  where
    annotations = List.sort [ name | TypeAnnotation name _ <- defs ]
    definitions = List.sort $ concatMap Set.toList [ boundVars pattern | Def pattern _ <- defs ]

    dups = map head . filter ((>1) . length) . List.group

    msg = "Syntax Error: There can only be one "
    defMsg x = msg ++ " definition of '" ++ x ++ "'."
    annMsg x = msg ++ " type annotation for '" ++ x ++ "'."


badOrder :: [Def t v] -> [String]
badOrder defs = go defs
    where
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."

      go defs =
          case defs of
            TypeAnnotation name _ : Def (PVar name') _ : rest
                | name == name' -> go rest

            TypeAnnotation name _  : rest -> [msg name] ++ go rest

            _ : rest -> go rest

            _ -> []


