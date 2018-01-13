{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Compiler.Imports
  ( Import(..)
  , Exposing(..)
  , defaults
  )
  where


import qualified Elm.Name as N



-- IMPORTS


data Import =
  Import
    { _name :: N.Name
    , _alias :: Maybe N.Name
    , _exposing :: Exposing
    }


data Exposing
  = Open
  | Closed
  | TypeOpen N.Name
  | TypeClosed N.Name
  | Operator N.Name



-- DEFAULTS


defaults :: [Import]
defaults =
    [ Import "Basics" Nothing Open
    , Import "Debug" Nothing Closed
    , Import "List" Nothing (Operator "::")
    , Import "Maybe" Nothing (TypeOpen "Maybe")
    , Import "Result" Nothing (TypeOpen "Result")
    , Import "String" Nothing (TypeClosed "String")
    , Import "Char" Nothing (TypeClosed "Char")
    , Import "Tuple" Nothing Closed
    , Import "Platform" Nothing (TypeClosed "Program")
    , Import "Platform.Cmd" (Just "Cmd") (TypeClosed "Cmd")
    , Import "Platform.Sub" (Just "Sub") (TypeClosed "Sub")
    ]
