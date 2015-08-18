{-# LANGUAGE OverloadedStrings #-}
module Elm.Docs where

import Control.Applicative ((<$>),(<*>),(<|>))
import Control.Monad
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Docs.AST as Docs
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Compiler.Version as Version
import qualified Reporting.Annotation as A
import qualified Reporting.Region as Region


data Documentation = Documentation
    { moduleName :: Module.Name
    , comment :: String
    , aliases :: [Alias]
    , types :: [Union]
    , values :: [Value]
    , version :: Version
    }


data Alias = Alias
    { aliasName :: String
    , aliasComment :: String
    , aliasArgs :: [String]
    , aliasType :: Type.Type
    , aliasSource :: String
    }


data Union = Union
    { unionName :: String
    , unionComment :: String
    , unionArgs :: [String]
    , unionCases :: [(String, [Type.Type])]
    , unionSource :: String
    }


data Value = Value
    { valueName :: String
    , valueComment :: String
    , valueType :: Type.Type
    , valueSource :: String
    , valueFix :: Maybe (String,Int)
    }


data Version = NonCanonicalTypes | Version String


-- FROM CHECKED DOCS

fromCheckedDocs :: String -> Module.Name -> Docs.Checked -> Documentation
fromCheckedDocs path name (Docs.Docs comment aliases unions values) =
  let
    source (Region.Region start _) =
      path ++ "#L" ++ show (Region.line start)

    unwrap cmnt =
      maybe "" id cmnt

    toAlias (name, (A.A region (Docs.Alias cmnt args tipe))) =
      Alias name (unwrap cmnt) args tipe (source region)

    toUnion (name, (A.A region (Docs.Union cmnt args cases))) =
      Union name (unwrap cmnt) args cases (source region)

    toValue (name, (A.A region (Docs.Value cmnt tipe fix))) =
      Value name (unwrap cmnt) tipe (source region) fix
  in
  Documentation name comment
    (map toAlias (Map.toList aliases))
    (map toUnion (Map.toList unions))
    (map toValue (Map.toList values))
    (Version Version.version)


-- PRETTY JSON

prettyJson :: (Json.ToJSON a) => a -> BS.ByteString
prettyJson value =
  Json.encodePretty' config value


config :: Json.Config
config =
    Json.Config
    { Json.confIndent = 2
    , Json.confCompare = Json.keyOrder keys
    }
  where
    keys =
        [ "tag", "name", "comment", "aliases", "types"
        , "values", "func", "args", "type", "cases"
        ]


-- JSON for DOCUMENTATION

instance Json.ToJSON Documentation where
    toJSON (Documentation name comment aliases types values version) =
        Json.object
        [ "name" .= name
        , "comment" .= comment
        , "aliases" .= aliases
        , "types" .= types
        , "values" .= values
        , "generated-with-elm-version" .= version
        ]


instance Json.FromJSON Documentation where
    parseJSON (Json.Object obj) =
        Documentation
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "aliases"
            <*> obj .: "types"
            <*> obj .: "values"
            <*> ((obj .: "generated-with-elm-version") <|> return NonCanonicalTypes)

    parseJSON value =
        fail $ "Cannot decode Documentation from: " ++ BS.unpack (Json.encode value)


-- JSON for VERSION

instance Json.ToJSON Version where
    toJSON version =
      case version of
        NonCanonicalTypes ->
          Json.String "old"

        Version vsn ->
          Json.String (Text.pack vsn)


instance Json.FromJSON Version where
    parseJSON (Json.String text) =
        case Text.unpack text of
          "old" -> return NonCanonicalTypes
          vrsn -> return (Version vrsn)

    parseJSON value =
        fail $ "Cannot decode version of documentation from: " ++ BS.unpack (Json.encode value)


-- JSON for ALIAS

instance Json.ToJSON Alias where
    toJSON (Alias name comment args tipe source) =
        Json.object
        [ "name" .= name
        , "comment" .= comment
        , "args" .= args
        , "type" .= tipe
        , "source" .= source
        ]

instance Json.FromJSON Alias where
    parseJSON (Json.Object obj) =
        Alias
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "args"
            <*> obj .: "type"
            <*> obj .: "source"

    parseJSON value =
        fail $ "Cannot decode Alias from: " ++ BS.unpack (Json.encode value)


-- JSON for UNION

instance Json.ToJSON Union where
    toJSON (Union name comment args cases source) =
        Json.object
        [ "name" .= name
        , "comment" .= comment
        , "args" .= args
        , "cases" .= cases
        , "source" .= source
        ]

instance Json.FromJSON Union where
    parseJSON (Json.Object obj) =
        Union
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "args"
            <*> obj .: "cases"
            <*> obj .: "source"

    parseJSON value =
        fail $ "Cannot decode Union from: " ++ BS.unpack (Json.encode value)


-- JSON for VALUE

instance Json.ToJSON Value where
    toJSON (Value name comment tipe source assocPrec) =
        Json.object (fields ++ possibleFields)
      where
        fields =
            [ "name" .= name
            , "comment" .= comment
            , "type" .= tipe
            , "source" .= source
            ]

        possibleFields =
            case assocPrec of
              Nothing -> []
              Just (assoc, prec) ->
                  [ "associativity" .= assoc
                  , "precedence" .= prec
                  ]


instance Json.FromJSON Value where
    parseJSON (Json.Object obj) =
        Value
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "type"
            <*> obj .: "source"
            <*> (liftM2 (,) <$> obj .:? "associativity" <*> obj .:? "precedence")

    parseJSON value =
        fail $ "Cannot decode Value from: " ++ BS.unpack (Json.encode value)

