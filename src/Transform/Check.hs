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
import qualified Transform.Canonicalize.Setup as Setup

import AST.PrettyPrint
import Elm.Utils ((|>))
import Text.PrettyPrint as P


mistakes :: [D.ValidDecl] -> [Doc]
mistakes decls =
    concat
      [ infiniteTypeAliases decls
      , illFormedTypes decls
      , map P.text (duplicateTypeDeclarations decls)
      , map P.text (duplicateValues decls)
      ]


dups :: Ord a => [a] -> [a]
dups names =
    List.sort names
      |> List.group
      |> filter ((>1) . length)
      |> map head


duplicateValues :: [D.ValidDecl] -> [String]
duplicateValues decls =
    map msg (dups (portNames ++ concatMap Pattern.boundVarList defPatterns)) ++
    case mapM exprDups (portExprs ++ defExprs) of
      Left name -> [msg name]
      Right _   -> []

  where
    msg x =
        "Name Collision: There can only be one definition of '" ++ x ++ "'."

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


duplicateTypeDeclarations :: [D.ValidDecl] -> [String]
duplicateTypeDeclarations decls = 
    map dupTypeError (dups (typeNames ++ aliasNames))
    ++ map dupCtorError (dups (ctorNames ++ aliasNames))
  where
    typeNames =
        [ name | D.Datatype name _ _ <- decls ]

    aliasNames =
        [ name | D.TypeAlias name _ _ <- decls ]

    ctorNames =
        concat [ map fst patterns | D.Datatype _ _ patterns <- decls ]


dupTypeError :: String -> String
dupTypeError name =
  "Name Collision: There can only be one type named '" ++ name ++ "'"


dupCtorError :: String -> String
dupCtorError name =
  "Name Collision: There can only be one constructor named '" ++ name ++ "'.\n"
  ++ "    Constructors are created for record type aliases and for union types, so\n"
  ++ "    something should be renamed or moved to a different module."


illFormedTypes :: [D.ValidDecl] -> [Doc]
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


infiniteTypeAliases :: [D.ValidDecl] -> [Doc]
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

      indented :: D.ValidDecl -> Doc
      indented decl = P.text "\n    " <> pretty decl <> P.text "\n"

      report name args tipe =
          P.vcat
            [ P.text $ eightyCharLines 0 msg1
            , indented $ D.TypeAlias name args tipe
            , P.text $ eightyCharLines 0 Setup.typeAliasErrorSegue
            , indented $ D.Datatype name args [(name,[tipe])]
            , P.text $ eightyCharLines 0 Setup.typeAliasErrorExplanation ++ "\n"
            ]
          where
            msg1 =
                "Type alias '" ++ name ++ "' is an infinite type. " ++
                "Notice that it appears in its own definition, so when \
                \you expand it, it just keeps getting bigger:"
