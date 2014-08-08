{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Internal.Documentation where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), keyOrder)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Text (Text, pack)
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint as P

import qualified AST.Annotation as A
import qualified AST.Declaration as Decl
import qualified AST.Helpers as Help
import qualified AST.Module as M
import qualified AST.PrettyPrint as PP
import qualified AST.Type as T
import qualified AST.Variable as Var
import qualified Transform.Localize as Localize

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
    , exposed :: Bool
    , body :: Map Text Value
    } deriving (Show)

generateDocumentation :: M.CanonicalModule -> BS.ByteString
generateDocumentation modul =
    encodePretty' config (generateModuleDoc modul)
  where
    config :: Config
    config = Config { confIndent = 2, confCompare = keyOrder keys }

    keys = [ "tag", "name", "document", "comment", "raw", "aliases", "datatypes"
           , "values", "typeVariables", "type", "constructors"
           ]

generateModuleDoc :: M.CanonicalModule -> ModuleDocument
generateModuleDoc modul =
  let b = M.body modul in
  ModuleDocument
    { name = intercalate "." $ M.names modul
    , document = fromMaybe "" $ M.comment modul
    , values = processAll (generateValueDoc (buildFixityMap $ M.fixities b)) (M.types b)
    , aliases = processAll generateAliasDoc $ M.aliases b
    , datatypes = processAll generateDatatypeDoc $ M.datatypes b
    }
  where
    processAll fn = map (uncurry fn) . Map.toList

    buildFixityMap = Map.fromList . map f
      where f (A.A _ (x, y, z)) = (z, (x, y))

    allImports = Localize.environment modul
    prettyType = PP.pretty . Localize.tipe allImports

    generateValueDoc :: Map String (Decl.Assoc, Int) -> String
                     -> A.Commented T.CanonicalType -> Document
    generateValueDoc fixityMap n (A.A doc tipe) =
      let prettyName =
              if Help.isOp n
                then "(" ++ n ++ ")"
                else n

          fixityPart =
              case Map.lookup n fixityMap of
                Nothing -> []
                Just (assoc, p) ->
                    [ ("associativity", toJSON $ show assoc)
                    , ("precedence", toJSON p)
                    ]
      in
      Document { docName = n
               , raw = P.render $ P.hsep [P.text prettyName, P.colon, prettyType tipe]
               , comment = fromMaybe "" doc
               , exposed = Set.member n exportedValues
               , body = Map.fromList $
                        ("type", typeToJSON tipe) : fixityPart
               }

    generateAliasDoc :: String -> A.Commented ([String], T.CanonicalType) -> Document
    generateAliasDoc n (A.A doc (vars, tipe)) =
      Document { docName = n
               , raw = P.render $ P.hsep $ (P.text "type" : P.text n : map PP.pretty vars) ++ [P.text "=", prettyType tipe]
               , comment = fromMaybe "" doc
               , exposed = Set.member n exportedAliases
               , body = Map.fromList
                        [ ("typeVariables", toJSON vars)
                        , ("type", typeToJSON tipe) ]
               }

    prettyCtors ls =
      case ls of
        [] -> []
        (x : xs) -> P.text "=" P.<+> prettyCtor x :
                    map ((P.text "|" P.<+>) . prettyCtor) xs

      where
        prettyCtor :: (String, [T.CanonicalType]) -> P.Doc
        prettyCtor (n, types) =
          P.hsep (P.text n : map prettyType types)

    generateDatatypeDoc :: String -> A.Commented (M.AdtInfo String) -> Document
    generateDatatypeDoc n (A.A doc (vars, ctors)) =
      let var = T.Type . Var.Canonical Var.Local
          tipe = T.App (var n) (map var vars)
          listing = Map.lookup n exportedDatas
      in
      Document { docName = n
               , raw = P.render (P.hsep (P.text "data" : P.text n : map PP.pretty vars) P.$$
                                 (P.nest 4 $ P.vcat (prettyCtors ctors)))
               , comment = fromMaybe "" doc
               , exposed = isJust listing
               , body = Map.fromList
                        [ ("typeVariables", toJSON vars)
                        , ("constructors", toJSON $ map (ctorToJSON tipe listing) ctors) ]
               }

    ctorToJSON :: T.CanonicalType -> Maybe (Var.Listing String) -> (String, [T.CanonicalType]) -> Value
    ctorToJSON tipe lst (ctor, tipes) =
      object [ "name" .= ctor
             , "type" .= (typeToJSON $ foldr T.Lambda tipe tipes)
             , "exposed" .=
               case lst of
                 Just l | Var._open l || ctor `elem` Var._explicits l -> True
                 _ -> False
             ]

    (exportedValues, exportedAliases, exportedDatas) = analyzeExports (M.exports modul)

    analyzeExports :: [Var.Value] -> (Set String, Set String, Map String (Var.Listing String))
    analyzeExports = go Set.empty Set.empty Map.empty
      where go vs as datas ls =
              case ls of
                [] -> (vs, as, datas)

                Var.Value value : rest ->
                  go (Set.insert value vs) as datas rest

                Var.Alias value : rest ->
                  go vs (Set.insert value as) datas rest

                Var.ADT value lst : rest ->
                  go vs as (Map.insert value lst datas) rest
    
instance ToJSON ModuleDocument

instance ToJSON Document where
  toJSON doc =
    let processPair (x, y) = x .= y in
    object $ [ "name" .= docName doc
             , "raw" .= raw doc
             , "comment" .= comment doc
             , "exposed" .= exposed doc
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
        , "name" .= toJSON (Var.toString n)
        , "args" .= map typeToJSON ts
        ]

      T.App _ _ -> error "Elm.Internal.Documentation: cannot serialize types with higher-kinded polymorphism yet"

      T.Type var ->
        [ "tag" .= ("adt" :: Text)
        , "name" .= toJSON (Var.toString var)
        , "args" .= map typeToJSON []
        ]

      T.Aliased alias _ ->
        [ "tag" .= ("alias" :: Text)
        , "alias" .= toJSON (Var.toString alias)
        ]

      T.Record fields ext ->
        [ "tag" .= ("record" :: Text)
        , "extension" .= toJSON (fmap typeToJSON ext)
        , "fields" .= toJSON (map field fields)
        ]
        where
          field (n,t) =
              object [ "name" .= pack n
                     , "type" .= typeToJSON t
                     ]

