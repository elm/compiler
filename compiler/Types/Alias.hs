{-# LANGUAGE PatternGuards #-}
module Types.Alias (dealias, get, mistakes) where

import SourceSyntax.Everything
import Control.Arrow (second)
import Data.List (group,sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types.Substitutions (subst)
import Types.Types
import Data.Data
import Data.Generics.Uniplate.Data

builtins :: [(String,([X],Type))]
builtins =
    let touch = ("t0", time) : map (flip (,) int) ["x","y","x0","y0","id"]
        state = [("string", string), ("selectionStart", int), ("selectionEnd", int)]
        line  = [("color", tipe "Color"), ("width", float),
                 ("cap", tipe "LineCap"), ("join", tipe "LineJoin"),
                 ("miterLimit", float),   ("dashing", listOf int),
                 ("dashOffset", int)]
        makeRecord fields =
            RecordT (Map.fromList $ map (second (:[])) fields) EmptyRecord
    in  [ ("String", ([], listOf char)),
          ("Time", ([], float)),
          ("KeyCode", ([], int)),
          ("Touch", ([], makeRecord touch)),
          ("FieldState", ([], makeRecord state)),
          ("LineStyle", ([], makeRecord line))
        ]

type AliasDict = Map.Map String ([X],Type)

get :: [Declaration t v] -> AliasDict
get decl = Map.fromList (builtins ++ concatMap getAlias decl)
    where
      getAlias decl = case decl of
                        TypeAlias alias xs t -> [(alias, (xs,t))]
                        _ -> []

dealias :: AliasDict -> Type -> Type
dealias aliases t =
  let f = dealias aliases in
  case t of
    ADT name ts -> case Map.lookup name aliases of
                     Just (xs,t) -> f (subst (zip xs ts) t)
                     Nothing -> ADT name (map f ts)
    LambdaT t u -> LambdaT (f t) (f u)
    RecordT r t -> RecordT (Map.map (map f) r) (f t)
    _ -> t

mistakes :: (Data t, Data v) => [Declaration t v] -> [String]
mistakes decls = badKinds decls ++ dups decls ++ badOrder decls

badKinds :: (Data t, Data v) => [Declaration t v] -> [String]
badKinds decls = map msg (concatMap bad decls)
  where
    msg x = "Type Error: Type alias '" ++ x ++
            "' was given the wrong number of arguments."

    aliases :: AliasDict
    aliases = get decls

    badType :: Type -> [String]
    badType t =
        case t of
          ADT name ts ->
              case Map.lookup name aliases of
                Just (xs,t) | length xs == length ts -> []
                            | otherwise -> [name]
                Nothing -> concatMap badType ts
          LambdaT t u -> badType t ++ badType u
          RecordT r t -> badType t ++ concatMap badType (concat (Map.elems r))
          _ -> []

    --badDef :: AliasDict -> Def t v -> [String]
    badDef def =
        case def of
          TypeAnnotation _ tipe -> badType tipe
          FnDef _ _ expr ->
              concatMap badDef $ concat [defList | Let defList _ <- universeBi expr]
          OpDef _ _ _ expr ->
              concatMap badDef $ concat [defList | Let defList _ <- universeBi expr]

    --bad :: Declaration t v -> [String]
    bad s =
      case s of
        Datatype _ _ tcs -> concatMap badType (concatMap snd tcs)
        ExportEvent _ _ tipe   -> badType tipe
        ImportEvent _ _ _ tipe -> badType tipe
        TypeAlias _ _ tipe     -> badType tipe
        Definition d -> badDef d

annotation :: Declaration t v -> [String]
annotation s =
    case s of
      Definition (TypeAnnotation name _) -> [name]
      _ -> []

definition :: Declaration t v -> [String]
definition s =
    case s of
      Definition d -> defName d
      _ -> []

defName :: Def t v -> [String]
defName d =
    case d of
      FnDef n _ _   -> [n]
      OpDef n _ _ _ -> [n]
      _ -> []

dups :: [Declaration t v] -> [String]
dups decls = map defMsg (dup definition) ++ map annMsg (dup annotation)
    where
      --dup :: (Declaration t v -> [String]) -> [String]
      dup f = map head . filter ((>1) . length) . group . sort $ concatMap f decls

      msg = "Syntax Error: There can only be one "
      defMsg x = msg ++ "top-level definition of '" ++ x ++ "'."
      annMsg x = msg ++ "type annotation for '" ++ x ++ "'."

badOrder :: (Data t, Data v) => [Declaration t v] -> [String]
badOrder decls =
    map msg $ missings (sort $ expectedPairs as ds) (sort $ actualPairs decls)
    where
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."
      as = sort $ concatMap annotation decls
      ds = sort $ concatMap definition decls

      expectedPairs :: [String] -> [String] -> [String]
      expectedPairs as ds =
          case (as,ds) of
            (x:xs, y:ys) -> case compare x y of
                              LT -> expectedPairs xs (y:ys)
                              EQ -> x : expectedPairs xs ys
                              GT -> expectedPairs (x:xs) ys
            ( _  ,  _  ) -> []

      --actualPairs :: [Def t v] -> [String]
      actualPairs decls =      
          case decls of
            Definition (TypeAnnotation n _) : Definition (FnDef m _ _) : rest ->
                (if n == m then [n] else []) ++ actualPairs rest
            Definition (TypeAnnotation n _) : Definition (OpDef m _ _ _) : rest ->
                (if n == m then [n] else []) ++ actualPairs rest
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