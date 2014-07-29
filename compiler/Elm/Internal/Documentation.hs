{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -W -Wall #-}
module Elm.Internal.Documentation where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.Aeson
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as Map

import qualified AST.Annotation as A
import qualified AST.Declaration as Decl
import qualified AST.Helpers as Help
import qualified AST.Module as M
import qualified AST.PrettyPrint as PP
import qualified AST.Type as T
import qualified AST.Variable as Var

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

joinMapValues :: Ord k => Map k v1 -> Map k v2 -> Map k (v1, Maybe v2)
joinMapValues map1 map2 = Map.mapWithKey f map1
  where f key value = (value, Map.lookup key map2)

generateDocumentation :: M.CanonicalModule -> ModuleDocument
generateDocumentation modul =
  let b = M.body modul in
  ModuleDocument
    { name = intercalate "." $ M.names modul
    , document = fromMaybe "" $ M.comment modul
    , values = processAll generateValueDoc $ joinMapValues (M.types b) (M.defDocs b)
    , aliases = processAll generateAliasDoc $ M.aliases b
    , datatypes = processAll generateDatatypeDoc $ M.datatypes b
    }
  where
    processAll fn = map (uncurry fn) . Map.toList

    generateValueDoc :: String -> (T.CanonicalType, Maybe String) -> Document
    generateValueDoc n (tipe, doc) =
      let prettyName = if Help.isOp n
                         then "(" ++ n ++ ")"
                         else n
      in
      Document { docName = n
               , raw = concat [prettyName, " : ", PP.renderPretty tipe]
               , comment = fromMaybe "" doc
               , body = Map.fromList
                        [ ("type", typeToJSON tipe) ]
               }

    generateAliasDoc :: String -> Decl.AnnotatedDecl ([String], T.CanonicalType) -> Document
    generateAliasDoc n (A.A doc (vars, tipe)) =
      Document { docName = n
               , raw = concat ["type ", n, concatMap (' ':) vars, " = ", PP.renderPretty tipe]
               , comment = fromMaybe "" doc
               , body = Map.fromList
                        [ ("typeVariables", toJSON vars)
                        , ("type", typeToJSON tipe) ]
               }

    prettyCtors ls =
      case ls of
        [] -> ""
        (x : xs) -> prettyCtor "  =" x ++
                    concatMap ((++ "\n") . prettyCtor "  |") xs
      where
        prettyCtor :: String -> (String, [T.CanonicalType]) -> String
        prettyCtor prefix (n, types) =
          concat [prefix, " ", n, concatMap (("\n" ++) . PP.renderPretty) types]

    generateDatatypeDoc :: String -> Decl.AnnotatedDecl (M.AdtInfo String) -> Document
    generateDatatypeDoc n (A.A doc (vars, ctors)) =
      let var = T.Type . Var.Canonical Var.Local
          tipe = T.App (var n) (map var vars) in
      Document { docName = n
               , raw = concat ["data ", n, concatMap (' ':) vars, "\n"] ++
                       prettyCtors ctors
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

      T.App (T.Type n) ts ->
        [ "tag" .= ("adt" :: Text)
        , "name" .= toJSON (Var.name n)
        , "args" .= map typeToJSON ts
        ]

      T.App _ _ -> error "Elm.Internal.Documentation: incorrect type in function position"

      T.Record fields ext ->
        [ "tag" .= ("record" :: Text)
        , "extension" .= toJSON (typeToJSON <$> ext)
        , "field" .= toJSON (map (toJSON . second typeToJSON) fields)
        ]

      T.Type var ->
        [ "tag" .= ("adt" :: Text)
        , "name" .= (toJSON $ Var.name var)
        , "args" .= map typeToJSON []
        ]

      T.Aliased alias typ ->
        [ "tag" .= ("alias" :: Text)
        , "alias" .= toJSON (Var.name alias)
        , "type" .= typeToJSON typ
        ]
