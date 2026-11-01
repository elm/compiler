{-# LANGUAGE OverloadedStrings #-}
module Type.Occurs
  ( occurs
  )
  where


import qualified Data.List as List
import qualified Data.NonEmptyMap as NEM

import qualified Array as A

import Type.Type as Type
import qualified Type.UnionFind as UF



-- OCCURS
--
-- PERF try to do occurs check in a better way?


occurs :: Type.Variable -> IO Bool
occurs var =
  varOccurs [] var False


varOccurs :: [Type.Variable] -> Type.Variable -> Bool -> IO Bool
varOccurs seen var hasCycle =
  if List.elem var seen
  then return True
  else
    do  (Descriptor content _ _ _) <- UF.get var
        case content of
          Flex               _ -> return hasCycle
          Rigid              _ -> return hasCycle
          Error                -> return hasCycle
          Alias _ _ _ args _ _ -> A.foldlM (varOccurs (var:seen)) hasCycle args
          Structure term       -> termOccurs (var:seen) term hasCycle


termOccurs :: [Type.Variable] -> Type.FlatType -> Bool -> IO Bool
termOccurs seen term hasCycle =
  case term of
    App1 _ _ _ xs  -> A.foldlM (varOccurs seen) hasCycle xs
    Fun1 a b       -> varOccurs seen a =<< varOccurs seen b hasCycle
    Unit1          -> return hasCycle
    Pair1   a b    -> varOccurs seen a =<< varOccurs seen b hasCycle
    Triple1 a b c  -> varOccurs seen a =<< varOccurs seen b =<< varOccurs seen c hasCycle
    Empty1         -> return hasCycle
    Record1 _ fs   -> NEM.foldrM (varOccurs seen) hasCycle fs
    ExtRec1 _ fs e -> varOccurs seen e =<< NEM.foldrM (varOccurs seen) hasCycle fs
