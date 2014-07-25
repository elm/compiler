{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -W -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Internal.Documentation where

import Data.Aeson
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as Map

import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified AST.Module as M

type Type = T.CanonicalType

data ModuleDocument = ModuleDocument
    { name :: String
    , document :: String
    , values :: [Document]
    , aliases :: [Document]
    , datatypes :: [Document]
    } deriving (Generic)

data Document = Document
    { docName :: String
    , raw :: String
    , comment :: String
    , body :: Map Text Value
    } deriving (Show)

generateDocumentation :: M.CanonicalModule -> ModuleDocument
generateDocumentation modul = ModuleDocument
    { name = intercalate "." $ M.names modul
    , document = fromMaybe "" $ M.comment modul
    , values = []
    , aliases = []
    , datatypes = []
    }

instance ToJSON ModuleDocument

instance ToJSON Document where
  toJSON doc =
    let processPair (x, y) = x .= y in
    object $ [ "name" .= docName doc
             , "raw" .= raw doc
             , "comment" .= comment doc
             ] ++ map processPair (Map.toList $ body doc)

instance ToJSON var => ToJSON (T.Type var) where
  toJSON tipe =
    object $
    case tipe of
      T.Lambda _ _ ->
        let tipes = T.collectLambdas tipe in
        [ "tag" .= ("function" :: Text)
        , "args" .= toJSON (init tipes)
        , "result" .= toJSON (last tipes)
        ]

      T.Var x ->
        [ "tag" .= ("var" :: Text)
        , "name" .= toJSON x
        ]

      T.App n ts ->
        [ "tag" .= ("adt" :: Text)
        , "name" .= toJSON n
        , "args" .= map toJSON ts
        ]

      T.Record fields ext ->
        [ "tag" .= ("record" :: Text)
        , "extension" .= toJSON ext
        ]

      T.Type var ->
        [ "tag" .= ("type" :: Text) -- ???
        , "name" .= toJSON var
        ]

      T.Aliased _ typ ->
        [ "tag" .= ("aliased" :: Text) -- ???
        , "type" .= toJSON typ
        ]

instance ToJSON Var.Canonical where
  toJSON var = toJSON $ Var.name var

