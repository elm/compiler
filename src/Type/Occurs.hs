{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Occurs (occurs) where

import qualified Data.Map as Map

import Type.Type as Type
import qualified Type.UnionFind as UF



-- OCCURS


occurs :: Type.Variable -> IO Bool
occurs var =
  occursHelp [] var


occursHelp :: [Type.Variable] -> Type.Variable -> IO Bool
occursHelp seen var =
  if elem var seen then
      do  infiniteDescriptor <- UF.descriptor var
          UF.setDescriptor var (infiniteDescriptor { _content = Error })
          return True

  else
      do  desc <- UF.descriptor var
          case _content desc of
            Atom _ ->
                return False

            Var _ _ _ ->
                return False

            Error ->
                return False

            Alias _ args _ ->
                -- TODO is it okay to only check args?
                or <$> traverse (occursHelp (var:seen) . snd) args

            Structure term ->
                let
                  go = occursHelp (var:seen)
                in
                case term of
                  App1 a b ->
                      (||) <$> go a <*> go b

                  Fun1 a b ->
                      (||) <$> go a <*> go b

                  EmptyRecord1 ->
                      return False

                  Record1 fields ext ->
                      or <$> traverse go (ext : Map.elems fields)
