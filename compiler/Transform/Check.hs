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
    map prettify $ concatMap findErrors (getLets decls)
  where
    prettify = P.sep . map P.text . words
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
    map defMsg (dup definition) ++ map annMsg (dup annotation)
  where
    dup f = map head . filter ((>1) . length) . List.group . List.sort $ concatMap f defs

    msg = "Syntax Error: There can only be one "
    defMsg x = msg ++ " definition of '" ++ x ++ "'."
    annMsg x = msg ++ " type annotation for '" ++ x ++ "'."


badOrder :: [Def t v] -> [String]
badOrder defs =
    map msg $ missings (List.sort $ expectedPairs as ds) (List.sort $ actualPairs defs)
    where
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."
      as = List.sort $ concatMap annotation defs
      ds = List.sort $ concatMap definition defs

      expectedPairs :: [String] -> [String] -> [String]
      expectedPairs as ds =
          case (as,ds) of
            (x:xs, y:ys) -> case compare x y of
                              LT -> expectedPairs xs (y:ys)
                              EQ -> x : expectedPairs xs ys
                              GT -> expectedPairs (x:xs) ys
            ( _  ,  _  ) -> []

      actualPairs defs =
          case defs of
            TypeAnnotation name _ : Def (PVar name') _ : rest ->
                (if name == name' then [name] else []) ++ actualPairs rest
            t:s:rest -> actualPairs (s:rest)
            _ -> []

      missings :: [String] -> [String] -> [String]
      missings expected actual =
          case (expected, actual) of
            (e:es, a:as) -> case compare e a of
                              LT -> e : missings es (a:as)
                              EQ -> missings es as
                              GT -> a : missings (e:es) as
            ( [] ,  _  ) -> actual
            (  _ ,  [] ) -> expected


annotation :: Def t v -> [String]
annotation def =
    case def of
      TypeAnnotation name _ -> [name]
      _ -> []
           
definition :: Def t v -> [String]
definition def =
    case def of
      Def pattern _ -> Set.toList (boundVars pattern)
      _ -> []