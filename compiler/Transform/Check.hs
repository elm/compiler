module Transform.Check (mistakes) where

import Transform.SortDefinitions (boundVars)
import qualified SourceSyntax.Declaration as D
import SourceSyntax.Expression
import SourceSyntax.Pattern
import SourceSyntax.Location
import SourceSyntax.PrettyPrint
import qualified SourceSyntax.Type as T
import Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Text.PrettyPrint as P


mistakes :: [D.Declaration t v] -> [Doc]
mistakes decls =
    concat [ infiniteTypeAliases decls
           , illFormedTypes decls
           , map P.text (duplicateConstructors decls)
           , map P.text (concatMap findErrors (getLets decls))
           , badDerivations decls ]
  where
    findErrors defs = duplicates defs ++ badOrder defs

getLets :: [D.Declaration t v] -> [[Def t v]]
getLets decls = defs : concatMap defLets defs
    where
      defs = [ d | D.Definition d <- decls ]

      defLets def =
          case def of
            TypeAnnotation _ _ -> []
            Def _ expr -> exprLets expr

      exprLets (L _ expr) =
          case expr of
            Var _ -> []
            Lambda p e -> exprLets e
            Binop op e1 e2 -> exprLets e1 ++ exprLets e2
            Case e cases -> exprLets e ++ concatMap (exprLets . snd) cases
            Data name es -> concatMap exprLets es
            Literal _ -> []
            Range e1 e2 -> exprLets e1 ++ exprLets e2
            ExplicitList es -> concatMap exprLets es
            App e1 e2 -> exprLets e1 ++ exprLets e2
            MultiIf branches -> concatMap (\(b,e) -> exprLets b ++ exprLets e) branches
            Access e lbl -> exprLets e
            Remove e lbl -> exprLets e
            Insert e lbl v -> exprLets e ++ exprLets v
            Modify e fields -> exprLets e ++ concatMap (exprLets . snd) fields
            Record fields -> concatMap (exprLets . snd) fields
            Markdown uid md es -> []
            Let defs body -> [defs] ++ exprLets body

dups :: Eq a => [a] -> [a]
dups  = map head . filter ((>1) . length) . List.group

dupErr :: String -> String -> String
dupErr err x = 
  "Syntax Error: There can only be one " ++ err ++ " '" ++ x ++ "'."

duplicates :: [Def t v] -> [String]
duplicates defs =
    map defMsg (dups definitions) ++ map annMsg (dups annotations)
  where
    annotations = List.sort [ name | TypeAnnotation name _ <- defs ]
    definitions = List.sort $ concatMap Set.toList [ boundVars pattern | Def pattern _ <- defs ]
    defMsg = dupErr "definition of"
    annMsg = dupErr "type annotation for"

duplicateConstructors :: [D.Declaration t v] -> [String]
duplicateConstructors decls = 
    map typeMsg (dups typeCtors) ++ map dataMsg (dups dataCtors)
  where
    typeCtors = List.sort [ name | D.Datatype name _ _ _ <- decls ]
    dataCtors = List.sort . concat $
      [ map fst patterns | D.Datatype _ _ patterns _ <- decls ]
    dataMsg = dupErr "definition of data constructor"
    typeMsg = dupErr "definition of type constructor"

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

badDerivations :: [D.Declaration t v] -> [Doc]
badDerivations decls = concatMap badDerivation derivations
    where
      derivations =
          [ (decl, tvars, derives) | decl@(D.TypeAlias name tvars _ derives) <- decls ] ++
          [ (decl, tvars, derives) | decl@(D.Datatype name tvars _ derives) <- decls ]

      badDerivation (decl, tvars, derives) =
          case (tvars, derives) of
            (_:_, _)
                | D.Json   `elem` derives -> [report decl D.Json]
                | D.Binary `elem` derives -> [report decl D.Binary]
            _ -> []

      report decl derive =
          P.vcat [ P.text $ "Error: cannot derive '" ++ show derive ++ "' from this type alias."
                 , P.text "Make sure all type variables are replaced with concrete types:"
                 , P.text "\n"
                 , nest 4 (pretty decl)
                 ]

illFormedTypes :: [D.Declaration t v] -> [Doc]
illFormedTypes decls = map report (Maybe.mapMaybe isIllFormed (aliases ++ adts))
    where
      aliases = [ (decl, tvars, [tipe]) | decl@(D.TypeAlias _ tvars tipe _) <- decls ]
      adts = [ (decl, tvars, concatMap snd ctors) | decl@(D.Datatype _ tvars ctors _) <- decls ]

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
                  _ -> "s" ++ addCommas (map ((++) " ") (addAnd (map quote tvars))) ++ " are"

            addCommas xs
                | length xs < 3 = concat xs
                | otherwise = intercalate "," xs

            addAnd xs
                | length xs < 2 = xs
                | otherwise = zipWith (++) (replicate (length xs - 1) "" ++ ["and "]) xs

            quote tvar = "'" ++ tvar ++ "'"


infiniteTypeAliases :: [D.Declaration t v] -> [Doc]
infiniteTypeAliases decls =
    [ report decl | decl@(D.TypeAlias name _ tipe _) <- decls, isInfinite name tipe ]
    where
      isInfinite name tipe =
          let infinite = isInfinite name in
          case tipe of
            T.Lambda a b -> infinite a || infinite b
            T.Var _ -> False
            T.Data name' ts -> name == name' || any infinite ts
            T.EmptyRecord -> False
            T.Record fields ext -> infinite ext || any (infinite . snd) fields

      report decl@(D.TypeAlias name args tipe derivations) =
          P.vcat [ P.text $ eightyCharLines 0 msg1
                 , indented decl
                 , P.text $ eightyCharLines 0 msg2
                 , indented (D.Datatype name args [(name,[tipe])] derivations)
                 , P.text $ eightyCharLines 0 msg3 ++ "\n"
                 ]
          where
            indented decl = P.text "\n    " <> pretty decl <> P.text "\n"

            msg1 = "Type alias '" ++ name ++ "' is an infinite type. " ++
                   "Notice that it appears in its own definition, so when \
                   \you expand it, it just keeps getting bigger:"
            msg2 = "Try this instead:"
            msg3 = "It looks very similar, but an algebraic data type (ADT) \
                   \actually creates a new type. Unlike with a type alias, this \
                   \freshly created type is meaningful on its own, so an ADT \
                   \does not need to be expanded."
