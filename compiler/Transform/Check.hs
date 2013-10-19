module Transform.Check (mistakes) where

import Transform.SortDefinitions (boundVars)
import SourceSyntax.Everything
import SourceSyntax.PrettyPrint
import qualified SourceSyntax.Type as T
import Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Data
import Data.Generics.Uniplate.Data
import Text.PrettyPrint as P


mistakes :: (Data t, Data v) => [Declaration t v] -> [Doc]
mistakes decls =
    illFormedTypes decls ++ map P.text (concatMap findErrors (getLets decls))
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
    defMsg x = msg ++ "definition of '" ++ x ++ "'."
    annMsg x = msg ++ "type annotation for '" ++ x ++ "'."


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

illFormedTypes :: [Declaration t v] -> [Doc]
illFormedTypes decls = map report (Maybe.mapMaybe isIllFormed (aliases ++ adts))
    where
      aliases = [ (decl, tvars, [tipe]) | decl@(TypeAlias _ tvars tipe) <- decls ]
      adts = [ (decl, tvars, concatMap snd ctors) | decl@(Datatype _ tvars ctors) <- decls ]

      freeVars tipe =
          case tipe of
            T.Lambda t1 t2 -> Set.union (freeVars t1) (freeVars t2)
            T.Var x -> Set.singleton x
            T.Data _ ts -> Set.unions (map freeVars ts)
            T.EmptyRecord -> Set.empty
            T.Record fields ext -> Set.unions (freeVars ext : map (freeVars . snd) fields)

      undeclared tvars tipes = Set.difference used declared
          where
            used = Set.unions (map freeVars tipes)
            declared = Set.fromList tvars

      isIllFormed (decl, tvars, tipes) =
          let unbound = undeclared tvars tipes in 
          if Set.null unbound then Nothing
                              else Just (decl, Set.toList unbound)

      report (decl, tvars) =
          P.vcat [ P.text $ "Error: type variable" ++ listing ++ " unbound in:"
                 , P.text "\n"
                 , nest 4 (pretty decl) ]
          where
            listing =
                case tvars of
                  [tvar] -> " " ++ quote tvar ++ " is"
                  _      -> "s " ++ addCommas (addAnd (map quote tvars)) ++ " are"

            addCommas xs
                | length xs < 3 = concat xs
                | otherwise = intercalate ", " xs

            addAnd xs
                | length xs < 2 = xs
                | otherwise = zipWith (++) (replicate (length xs - 1) "" ++ ["and "]) xs

            quote tvar = "'" ++ tvar ++ "'"