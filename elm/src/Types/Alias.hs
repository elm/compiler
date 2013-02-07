
module Types.Alias (dealias, mistakes) where

import Ast
import Control.Arrow (second)
import Data.List (group,sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types.Substitutions (subst)
import Types.Types

builtins :: [(String,([X],Type))]
builtins = [ ("String", ([], string)) ]

getAliases :: [Statement] -> Map.Map String ([X],Type)
getAliases stmts = Map.fromList (builtins ++ concatMap getAlias stmts)
    where getAlias stmt = case stmt of
                            TypeAlias alias xs t -> [(alias, (xs,t))]
                            _ -> []


dealias :: [Statement] -> [Statement]
dealias stmts = map dealiasS stmts
  where
    dealiasT :: Type -> Type
    dealiasT t =
      case t of
        ADT name ts -> case Map.lookup name (getAliases stmts) of
                         Just (xs,t) -> dealiasT (subst (zip xs ts) t)
                         Nothing -> ADT name (map dealiasT ts)
        LambdaT t u -> LambdaT (dealiasT t) (dealiasT u)
        RecordT r t -> RecordT (Map.map (map dealiasT) r) (dealiasT t)
        _ -> t

    dealiasS :: Statement -> Statement
    dealiasS s =
      case s of
        Datatype n xs tcs -> Datatype n xs (map (second (map dealiasT)) tcs)
        ExportEvent js elm tipe   -> ExportEvent js elm (dealiasT tipe)
        ImportEvent js e elm tipe -> ImportEvent js e elm (dealiasT tipe)
        TypeAnnotation name tipe  -> TypeAnnotation name (dealiasT tipe)
        TypeAlias alias xs tipe   -> TypeAlias alias xs (dealiasT tipe)
        Definition _ -> s

mistakes :: [Statement] -> [String]
mistakes stmts = badKinds stmts ++ dups stmts ++ badOrder stmts

badKinds :: [Statement] -> [String]
badKinds stmts = map msg (concatMap badS stmts)
  where
    msg x = "Type Error: Type alias '" ++ x ++
            "' was given the wrong number of arguments."

    badT :: Type -> [String]
    badT t =
      case t of
        ADT name ts ->
          case Map.lookup name (getAliases stmts) of
            Just (xs,t) | length xs == length ts -> []
                        | otherwise -> [name]
            Nothing -> concatMap badT ts
        LambdaT t u -> badT t ++ badT u
        RecordT r t -> badT t ++ concatMap badT (concat (Map.elems r))
        _ -> []

    badS :: Statement -> [String]
    badS s =
      case s of
        Datatype _ _ tcs -> concatMap badT (concatMap snd tcs)
        ExportEvent _ _ tipe   -> badT tipe
        ImportEvent _ _ _ tipe -> badT tipe
        TypeAnnotation _ tipe  -> badT tipe
        TypeAlias _ _ tipe     -> badT tipe
        Definition _ -> []

annotation :: Statement -> [String]
annotation s =
    case s of
      TypeAnnotation name _ -> [name]
      _ -> []

definition :: Statement -> [String]
definition s =
    case s of
      Definition d -> [defName d]
      _ -> []

defName :: Def -> String
defName d =
    case d of
      FnDef n _ _   -> n
      OpDef n _ _ _ -> n

dups :: [Statement] -> [String]
dups stmts = map defMsg (dup definition) ++ map annMsg (dup annotation)
    where
      dup :: (Statement -> [String]) -> [String]
      dup f = map head . filter ((>1) . length) . group . sort $ concatMap f stmts

      msg = "Syntax Error: There can only be one "
      defMsg x = msg ++ "top-level definition of '" ++ x ++ "'."
      annMsg x = msg ++ "type annotation for '" ++ x ++ "'."

badOrder :: [Statement] -> [String]
badOrder stmts = map msg $ missings (sort $ expectedPairs as ds) (sort $ actualPairs stmts)
    where
      msg x = "Syntax Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."
      as = sort $ concatMap annotation stmts
      ds = sort $ concatMap definition stmts

      expectedPairs :: [String] -> [String] -> [String]
      expectedPairs as ds =
          case (as,ds) of
            (x:xs, y:ys) -> case compare x y of
                              LT -> expectedPairs xs (y:ys)
                              EQ -> x : expectedPairs xs ys
                              GT -> expectedPairs (x:xs) ys
            ( _  ,  _  ) -> []

      actualPairs :: [Statement] -> [String]
      actualPairs stmts =      
          case stmts of
            TypeAnnotation n _ : Definition d : rest ->
                if n == defName d then [n] else [] ++ actualPairs rest
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