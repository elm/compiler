{-# LANGUAGE OverloadedStrings #-}
module Elm.Docs where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy.Char8 as BS

import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type


data Documentation = Documentation
    { moduleName :: Module.Name
    , comment :: String
    , aliases :: [Alias]
    , types :: [Union]
    , values :: [Value]
    }


data Alias = Alias
    { aliasName :: String
    , aliasComment :: String
    , aliasArgs :: [String]
    , aliasType :: Type.Type
    }


data Union = Union
    { unionName :: String
    , unionComment :: String
    , unionArgs :: [String]
    , unionCases :: [(String, [Type.Type])]
    }


data Value = Value
    { valueName :: String
    , valueComment :: String
    , valueType :: Type.Type
    , valueAssocPrec :: Maybe (String,Int)
    }


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
    toJSON (Documentation name comment aliases types values) =
        Json.object
        [ "name" .= name
        , "comment" .= comment
        , "aliases" .= aliases
        , "types" .= types
        , "values" .= values
        ]


instance Json.FromJSON Documentation where
    parseJSON (Json.Object obj) =
        Documentation
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "aliases"
            <*> obj .: "types"
            <*> obj .: "values"

    parseJSON value =
        fail $ "Cannot decode Documentation from: " ++ BS.unpack (Json.encode value)


-- JSON for ALIAS

instance Json.ToJSON Alias where
    toJSON (Alias name comment args tipe) =
        Json.object
        [ "name" .= name
        , "comment" .= comment
        , "args" .= args
        , "type" .= tipe
        ]

instance Json.FromJSON Alias where
    parseJSON (Json.Object obj) =
        Alias
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "args"
            <*> obj .: "type"

    parseJSON value =
        fail $ "Cannot decode Alias from: " ++ BS.unpack (Json.encode value)


-- JSON for UNION

instance Json.ToJSON Union where
    toJSON (Union name comment args cases) =
        Json.object
        [ "name" .= name
        , "comment" .= comment
        , "args" .= args
        , "cases" .= cases
        ]

instance Json.FromJSON Union where
    parseJSON (Json.Object obj) =
        Union
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "args"
            <*> obj .: "cases"

    parseJSON value =
        fail $ "Cannot decode Union from: " ++ BS.unpack (Json.encode value)


-- JSON for VALUE

instance Json.ToJSON Value where
    toJSON (Value name comment tipe assocPrec) =
        Json.object (fields ++ possibleFields)
      where
        fields =
            [ "name" .= name
            , "comment" .= comment
            , "type" .= tipe
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
            <*> (liftM2 (,) <$> obj .:? "associativity" <*> obj .:? "precedence")

    parseJSON value =
        fail $ "Cannot decode Value from: " ++ BS.unpack (Json.encode value)

