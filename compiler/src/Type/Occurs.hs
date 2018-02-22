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
  if elem var seen then
    return True

  else
    do  (Descriptor content _ _ _) <- UF.get var
        case content of
          FlexVar _ ->
              return foundCycle

          FlexSuper _ _ ->
              return foundCycle

          RigidVar _ ->
              return foundCycle

          RigidSuper _ _ ->
              return foundCycle

          Structure term ->
              let newSeen = var : seen in
              case term of
                App1 _ _ args ->
                    foldrM (occursHelp newSeen) foundCycle args

                Fun1 a b ->
                    occursHelp newSeen a =<<
                      occursHelp newSeen b foundCycle

                EmptyRecord1 ->
                    return foundCycle

                Record1 fields ext ->
                    occursHelp newSeen ext =<<
                      foldrM (occursHelp newSeen) foundCycle (Map.elems fields)

                Unit1 ->
                    return foundCycle

                Tuple1 a b maybeC ->
                    case maybeC of
                      Nothing ->
                        occursHelp newSeen a =<<
                          occursHelp newSeen b foundCycle

                      Just c ->
                        occursHelp newSeen a =<<
                          occursHelp newSeen b =<<
                            occursHelp newSeen c foundCycle

          Alias _ _ args _ ->
              foldrM (occursHelp (var:seen)) foundCycle (map snd args)

          Error ->
              return foundCycle
