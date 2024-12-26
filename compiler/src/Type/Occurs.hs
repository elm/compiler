{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Occurs
  ( occurs
  )
  where


import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map

import Type.Type as Type
import qualified Type.UnionFind as UF



-- OCCURS


occurs :: Type.Variable -> IO Bool
occurs var =
  occursHelp [] var False


occursHelp :: [Type.Variable] -> Type.Variable -> Bool -> IO Bool
occursHelp seen var foundCycle =
  if var `elem` seen then
    return True
  else
    do  (Descriptor content _ _ _ _) <- UF.get var
        case content of
          FlexVar _ -> return foundCycle
          FlexSuper _ _ -> return foundCycle
          RigidVar _ -> return foundCycle
          RigidSuper _ _ -> return foundCycle
          Structure term ->
            case term of
              App1 _ _ args ->
                foldrM (occursHelp (var : seen)) foundCycle args

              Fun1 arg result ->
                do  cycleInArg <- occursHelp (var : seen) arg foundCycle
                    if cycleInArg
                      then return True
                      else occursHelp (var : seen) result foundCycle

              EmptyRecord1 ->
                return foundCycle

              Record1 fields extension ->
                do  cycleInFields <- foldrM (occursHelp (var : seen)) foundCycle (Map.elems fields)
                    if cycleInFields
                      then return True
                      else occursHelp (var : seen) extension foundCycle

              Unit1 ->
                return foundCycle

              Tuple1 a b maybeC ->
                case maybeC of
                  Nothing ->
                    do  cycleInA <- occursHelp (var : seen) a foundCycle
                        if cycleInA
                          then return True
                          else occursHelp (var : seen) b foundCycle

                  Just c ->
                    do  cycleInA <- occursHelp (var : seen) a foundCycle
                        if cycleInA
                          then return True
                          else do
                            cycleInB <- occursHelp (var : seen) b foundCycle
                            if cycleInB
                              then return True
                              else occursHelp (var : seen) c foundCycle

          Alias _ _ args _ ->
            foldrM (occursHelp (var : seen)) foundCycle (map snd args)

          Error ->
            return foundCycle
