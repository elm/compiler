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

import qualified AST.Annotation as A
import qualified AST.Declaration as Decl
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
    , aliases = processAll generateAliasDoc $ M.aliases $ M.body modul
    , datatypes = processAll generateDatatypeDoc $ M.datatypes $ M.body modul
    }
  where
    processAll fn = map (uncurry fn) . Map.toList

    generateAliasDoc :: String -> Decl.AnnotatedDecl ([String], T.CanonicalType) -> Document
    generateAliasDoc name (A.A doc (vars, tipe)) =
      Document { docName = name
               , raw = "???"
               , comment = fromMaybe "" doc
               , body = Map.fromList
                        [ ("typeVariables", toJSON vars)
                        , ("type", typeToJSON tipe) ]
               }

    generateDatatypeDoc :: String -> Decl.AnnotatedDecl (M.AdtInfo String) -> Document
    generateDatatypeDoc name (A.A doc (vars, ctors)) =
      let var = T.Type . Var.Canonical Var.Local
          tipe = T.App (var name) (map var vars) in
      Document { docName = name
               , raw = "???"
               , comment = fromMaybe "" doc
               , body = Map.fromList
                        [ ("typeVariables", toJSON vars)
                        , ("constructors", toJSON $ map (ctorToJSON tipe) ctors) ]
               }

    ctorToJSON :: T.CanonicalType -> (String, [T.CanonicalType]) -> Value
    ctorToJSON tipe (ctor, tipes) =
      object [ "name" .= ctor
             , "type" .= (typeToJSON $ foldr T.Lambda tipe tipes) ]
    
instance ToJSON ModuleDocument

instance ToJSON Document where
  toJSON doc =
    let processPair (x, y) = x .= y in
    object $ [ "name" .= docName doc
             , "raw" .= raw doc
             , "comment" .= comment doc
             ] ++ map processPair (Map.toList $ body doc)

typeToJSON :: T.CanonicalType -> Value
typeToJSON tipe =
  object $
    case tipe of
      T.Lambda _ _ ->
        let tipes = T.collectLambdas tipe in
        [ "tag" .= ("function" :: Text)
        , "args" .= map typeToJSON (init tipes)
        , "result" .= typeToJSON (last tipes)
        ]

      T.Var x ->
        [ "tag" .= ("var" :: Text)
        , "name" .= toJSON x
        ]

      T.App n ts ->
        [ "tag" .= ("adt" :: Text)
        , "name" .= typeToJSON n
        , "args" .= map typeToJSON ts
        ]

      T.Record fields ext ->
        [ "tag" .= ("record" :: Text)
        , "extension" .= ("FIXME" :: Text) -- toJSON ext
        ]

      T.Type var ->
        [ "tag" .= ("type" :: Text) -- ???
        , "name" .= toJSON var
        ]

      T.Aliased _ typ ->
        [ "tag" .= ("aliased" :: Text) -- ???
        , "type" .= typeToJSON typ
        ]

instance ToJSON Var.Canonical where
  toJSON var = toJSON $ Var.name var

