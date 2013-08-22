module Type.Alias (realias, rules, Rules) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Control.Arrow (second)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.List as List
import SourceSyntax.Type
import SourceSyntax.Module

type Rules = ([(String,[String],Type)], Type -> Type)

rules interfaces metaModule = (collect interfaces metaModule, localizer metaModule)

collect interfaces metaModule = filter (not . isPrimitive) rawAliases
  where
    rawAliases = aliases metaModule ++ concatMap iAliases (Map.elems interfaces)

    isPrimitive (_,_,tipe) =
        case tipe of
          Data _ [] -> True
          _ -> False

localizer metaModule = go
  where
    go tipe = 
        case tipe of
          Var _ -> tipe
          EmptyRecord -> tipe
          Lambda t1 t2 -> Lambda (go t1) (go t2)
          Data name ts -> Data (localize name) (map go ts)
          Record fs ext -> Record (map (second go) fs) (go ext)

    byMethod = foldr (\(n,m) d -> Map.insertWith (++) n [m] d) Map.empty (imports metaModule)

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
realias (aliases,localize) tipe = localize (realiasHelp aliases tipe)

realiasHelp :: [(String,[String],Type)] -> Type -> Type
realiasHelp aliases tipe =
    case concatMap tryRealias aliases of
      [tipe''] -> f tipe''
      _ -> if tipe == tipe' then tipe else f tipe'
  where
    tryRealias (name, args, t) =
        case diff t tipe of
          Nothing -> []
          Just kvs ->
              let holes = collectFields kvs
                  hasArgs = List.sort args == Map.keys holes
                  isConsistent = all allEqual (Map.elems holes)
              in  case hasArgs && isConsistent of
                    False -> []
                    True -> [Data name $ map (\arg -> head (holes ! arg)) args]

    f = realiasHelp aliases
    tipe' =
        case tipe of
          Var _ -> tipe
          EmptyRecord -> tipe
          Lambda t1 t2 -> Lambda (f t1) (f t2)
          Data name ts -> Data name (map f ts)
          Record fs ext -> Record (map (second f) fs) (f ext)

allEqual [] = True
allEqual (x:xs) = all (==x) xs

diff :: Type -> Type -> Maybe [(String,Type)]
diff general specific =
    case (general, specific) of
      (Lambda g1 g2, Lambda s1 s2) -> (++) <$> diff g1 s1 <*> diff g2 s2
      (Var x, t) -> Just [(x,t)]
      (Data gname gts, Data sname sts)
          | gname == sname && length gts == length sts ->
              concat <$> zipWithM diff gts sts
      (EmptyRecord, EmptyRecord) -> Just []
      (Record _ _, Record _ _) ->
          let (gfs,gext) = collectRecords general
              (sfs,sext) = collectRecords specific
              gfields = collectFields gfs
              sfields = collectFields sfs

              overlap = Map.intersectionWith (\gs ss -> length gs == length ss) sfields gfields
              isAligned = Map.size gfields == Map.size overlap && and (Map.elems overlap)
          in
              case isAligned of
                False -> Nothing
                True -> let remaining = Map.difference sfields gfields
                            sext' = if Map.null remaining then sext else
                                        Record (flattenFields remaining) sext
                            matchMap = Map.intersectionWith (zipWith diff) gfields sfields
                        in  concat <$> sequence (diff gext sext' : concat (Map.elems matchMap))
      (_,_) -> Nothing

collectFields fields =
    foldr (\(f,t) fs -> Map.insertWith (++) f [t] fs) Map.empty fields

flattenFields fields =
    concatMap (\(f,ts) -> map ((,) f) ts) (Map.toList fields)