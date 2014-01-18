{-# OPTIONS_GHC -W #-}
module Type.Alias (realias, rules, canonicalRealias, Rules) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Control.Arrow (second)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.List as List
import SourceSyntax.Type
import SourceSyntax.Module

type Rules = ([Alias], Type -> Type)

rules interfaces moduleAliases moduleImports =
    (collect interfaces moduleAliases, localizer moduleImports)

collect interfaces moduleAliases =
    filter (not . isPrimitive) rawAliases
    where
      rawAliases =
          moduleAliases ++ concatMap iAliases (Map.elems interfaces)

      isPrimitive (_,_,tipe,_) =
          case tipe of
          Data _ [] -> True
          _ -> False

localizer moduleImports = go
  where
    go tipe =
        case tipe of
          Var _ -> tipe
          Lambda t1 t2 -> Lambda (go t1) (go t2)
          Data name ts -> Data (localize name) (map go ts)
          Record fs ext -> Record (map (second go) fs) ext

    byMethod = foldr (\(n,m) d -> Map.insertWith (++) n [m] d)
               Map.empty moduleImports

    separate name =
        case List.elemIndices '.' name of
          [] -> ("",name)
          is -> let i = last is in
                (take i name, drop (i+1) name)

    shortest = List.minimumBy (\a b -> compare (length a) (length b))

    localize name = shortest (name : concatMap (localize' value) methods)
        where (modul, value) = separate name
              methods = Map.findWithDefault [] modul byMethod

    localize' name method =
        case method of
          As modul -> [modul ++ "." ++ name]
          Hiding xs | name `notElem` xs -> [name]
          Importing xs | name `elem` xs -> [name]
          _ -> []

realias :: Rules -> Type -> Type
realias (aliases,localize) tipe = localize (canonicalRealias aliases tipe)

-- Realias using canonical aliases, so results will have aliases
-- that are fully qualified and possible to compare.
canonicalRealias :: [Alias] -> Type -> Type
canonicalRealias aliases tipe =
    case concatMap tryRealias aliases of
      [] -> if tipe == tipe' then tipe else f tipe'
      tipes -> f (bestType tipes)
  where
    tryRealias (name, args, aliasTipe, _) =
        case diff aliasTipe tipe of
          Nothing -> []
          Just kvs ->
              let holes = collectFields kvs
                  hasArgs = List.sort args == Map.keys holes
                  isConsistent = all allEqual (Map.elems holes)
              in  case hasArgs && isConsistent of
                    False -> []
                    True -> [Data name $ map (\arg -> head (holes ! arg)) args]

    f = canonicalRealias aliases
    tipe' =
        case tipe of
          Var _ -> tipe
          Lambda t1 t2 -> Lambda (f t1) (f t2)
          Data name ts -> Data name (map f ts)
          Record fs ext -> Record (map (second f) fs) ext

allEqual [] = True
allEqual (x:xs) = all (==x) xs

bestType tipes = fst $ List.minimumBy (\a b -> compare (snd a) (snd b)) pairs
    where
      pairs :: [(Type,Int)]
      pairs = zip tipes (map numFields tipes)

      numFields :: Type -> Int
      numFields tipe =
          case tipe of
            Lambda t1 t2 -> numFields t1 + numFields t2
            Var _ -> 0
            Data _ ts -> sum (map numFields ts)
            Record fields _ -> length fields + sum (map (numFields . snd) fields)

diff :: Type -> Type -> Maybe [(String,Type)]
diff general specific =
    case (general, specific) of
      (Lambda g1 g2, Lambda s1 s2) -> (++) <$> diff g1 s1 <*> diff g2 s2
      (Var x, t) -> Just [(x,t)]
      (Data gname gts, Data sname sts)
          | gname == sname && length gts == length sts ->
              concat <$> zipWithM diff gts sts
      (Record [] Nothing, Record [] Nothing) -> Just []
      (Record _ _, Record [] Nothing) -> Nothing
      (Record [] Nothing, Record _ _) -> Nothing
      (Record gfs gext, Record sfs sext) ->
          let gfields = collectFields gfs
              sfields = collectFields sfs

              overlap = Map.intersectionWith (\gs ss -> length gs == length ss) sfields gfields
              isAligned = Map.size gfields == Map.size overlap && and (Map.elems overlap)
          in
              case isAligned of
                False -> Nothing
                True -> let remaining = Map.difference sfields gfields
                            sext' = case sext of
                                      Just x | Map.null remaining -> Var x
                                      _ -> Record (flattenFields remaining) sext
                            gext' = maybe (Record [] Nothing) Var gext
                            matchMap = Map.intersectionWith (zipWith diff) gfields sfields
                        in  concat <$> sequence (diff gext' sext' : concat (Map.elems matchMap))
      (_,_) -> Nothing

collectFields fields =
    foldr (\(f,t) fs -> Map.insertWith (++) f [t] fs) Map.empty fields

flattenFields fields =
    concatMap (\(f,ts) -> map ((,) f) ts) (Map.toList fields)