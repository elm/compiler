{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Occurs (occurs) where

import qualified Data.Map.Strict as Map

import Type.Type as Type
import qualified Type.UnionFind as UF



-- OCCURS


occurs :: Type.Variable -> IO Bool
occurs var =
  occursHelp [] var


occursHelp :: [Type.Variable] -> Type.Variable -> IO Bool
occursHelp seen var =
  if elem var seen then
    return True

  else
    do  (Descriptor content _ _ _) <- UF.descriptor var
        case content of
          FlexVar _ ->
              return False

          FlexSuper _ _ ->
              return False

          RigidVar _ ->
              return False

          RigidSuper _ _ ->
              return False

          Structure term ->
              let go = occursHelp (var:seen) in
              case term of
                App1 _ _ args ->
                    or <$> traverse go args

                Fun1 a b ->
                    (||) <$> go a <*> go b

                EmptyRecord1 ->
                    return False

                Record1 fields ext ->
                    or <$> traverse go (ext : Map.elems fields)

                Unit1 ->
                    return False

                Tuple1 a b cs ->
                    or <$> traverse go (a:b:cs)

          Alias _ _ args _ ->
              -- TODO is it okay to only check args?
              or <$> traverse (occursHelp (var:seen) . snd) args

          Error _ ->
              return False
