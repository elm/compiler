{-# OPTIONS_GHC -Wall #-}
module Transform.Check (mistakes) where

import qualified Control.Arrow as Arrow
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified AST.Expression.Valid as Valid
import qualified AST.Declaration as D
import qualified AST.Pattern as Pattern
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Transform.Expression as Expr

import AST.PrettyPrint
import Text.PrettyPrint as P


mistakes :: [D.ValidDecl'] -> [Doc]
mistakes decls =
    concat [ infiniteTypeAliases decls
           , illFormedTypes decls
           , map P.text (duplicateConstructors decls)
           , map P.text (duplicates decls) ]

dups :: Ord a => [a] -> [a]
dups  = map head . filter ((>1) . length) . List.group . List.sort

dupErr :: String -> String -> String
dupErr err x = 
  "Syntax Error: There can only be one " ++ err ++ " '" ++ x ++ "'."

duplicates :: [D.ValidDecl'] -> [String]
duplicates decls =
    map msg (dups (portNames ++ concatMap Pattern.boundVarList defPatterns)) ++
    case mapM exprDups (portExprs ++ defExprs) of
      Left name -> [msg name]
      Right _   -> []

  where
    msg = dupErr "definition of"

    (defPatterns, defExprs) =
        unzip [ (pat,expr) | D.Definition (Valid.Definition pat expr _) <- decls ]

    (portNames, portExprs) =
        Arrow.second concat $ unzip $ 
        flip map [ port | D.Port port <- decls ] $ \port ->
            case port of
              D.Out name expr _ -> (name, [expr])
              D.In name _ -> (name, [])

    exprDups :: Valid.Expr -> Either String Valid.Expr
    exprDups expr = Expr.crawlLet defsDups expr

    defsDups :: [Valid.Def] -> Either String [Valid.Def]
    defsDups defs =
        let varsIn (Valid.Definition pattern _ _) = Pattern.boundVarList pattern in
        case dups $ concatMap varsIn defs of
          []     -> Right defs
          name:_ -> Left name

duplicateConstructors :: [D.ValidDecl'] -> [String]
duplicateConstructors decls = 
    map (dupErr "definition of type constructor") (dups typeCtors) ++
    map (dupErr "definition of data constructor") (dups dataCtors)
  where
    typeCtors = [ name | D.Datatype name _ _ <- decls ]
    dataCtors = concat [ map fst patterns | D.Datatype _ _ patterns <- decls ]

illFormedTypes :: [D.ValidDecl'] -> [Doc]
illFormedTypes decls = map report (Maybe.mapMaybe isIllFormed (aliases ++ adts))
    where
      aliases = [ (decl, tvars, [tipe]) | decl@(D.TypeAlias _ tvars tipe) <- decls ]
      adts = [ (decl, tvars, concatMap snd ctors) | decl@(D.Datatype _ tvars ctors) <- decls ]

      freeVars tipe =
          case tipe of
            T.Lambda t1 t2 -> Set.union (freeVars t1) (freeVars t2)
            T.Var x -> Set.singleton x
            T.Type _ -> Set.empty
            T.App t ts -> Set.unions (map freeVars (t:ts))
            T.Record fields ext -> Set.unions (ext' : map (freeVars . snd) fields)
                where ext' = maybe Set.empty freeVars ext
            T.Aliased _ t -> freeVars t

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
                | otherwise = List.intercalate "," xs

            addAnd xs
                | length xs < 2 = xs
                | otherwise = zipWith (++) (replicate (length xs - 1) "" ++ ["and "]) xs

            quote tvar = "'" ++ tvar ++ "'"


infiniteTypeAliases :: [D.ValidDecl'] -> [Doc]
infiniteTypeAliases decls =
    [ report name tvars tipe | D.TypeAlias name tvars tipe <- decls
                             , infiniteType name tipe ]
    where
      infiniteType :: String -> T.Type Var.Raw -> Bool
      infiniteType name tipe =
          let infinite = infiniteType name in
          case tipe of
            T.Lambda a b -> infinite a || infinite b
            T.Var _ -> False
            T.Type (Var.Raw name') -> name == name'
            T.App t ts -> any infinite (t:ts)
            T.Record fields _ -> any (infinite . snd) fields
            T.Aliased _ t -> infinite t

      indented :: D.ValidDecl' -> Doc
      indented decl = P.text "\n    " <> pretty decl <> P.text "\n"

      report name args tipe =
          P.vcat [ P.text $ eightyCharLines 0 msg1
                 , indented $ D.TypeAlias name args tipe
                 , P.text $ eightyCharLines 0 msg2
                 , indented $ D.Datatype name args [(name,[tipe])]
                 , P.text $ eightyCharLines 0 msg3 ++ "\n"
                 ]
          where
            msg1 = "Type alias '" ++ name ++ "' is an infinite type. " ++
                   "Notice that it appears in its own definition, so when \
                   \you expand it, it just keeps getting bigger:"
            msg2 = "Try this instead:"
            msg3 = "It looks very similar, but an algebraic data type (ADT) \
                   \actually creates a new type. Unlike with a type alias, this \
                   \freshly created type is meaningful on its own, so an ADT \
                   \does not need to be expanded."
