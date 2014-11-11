{-# LANGUAGE OverloadedStrings #-}
module Elm.Docs where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (second)
import Control.Monad
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as Text

import qualified Elm.Compiler.Module as Module


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
    , aliasType :: Type
    }


data Union = Union
    { unionName :: String
    , unionComment :: String
    , unionArgs :: [String]
    , unionCases :: [(String, [Type])]
    }


data Value = Value
    { valueName :: String
    , valueComment :: String
    , valueType :: Type
    , valueAssocPrec :: Maybe (String,Int)
    }


data Type
    = Lambda Type Type
    | Var String
    | Type String
    | App Type [Type]
    | Record [(String, Type)] (Maybe Type)


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


-- JSON for TYPE

instance Json.ToJSON Type where
    toJSON tipe =
        Json.object (getFields tipe)
      where
        getFields tipe =
            case tipe of
              Lambda t1 t2 ->
                  [ "tag" .= ("lambda" :: Text.Text)
                  , "in" .= Json.toJSON t1
                  , "out" .= Json.toJSON t2
                  ]

              Var x ->
                  [ "tag" .= ("var" :: Text.Text)
                  , "name" .= Json.toJSON x
                  ]

              Type name ->
                  [ "tag" .= ("type" :: Text.Text)
                  , "name" .= Json.toJSON name
                  ]

              App t ts -> 
                  [ "tag" .= ("app" :: Text.Text)
                  , "func" .= Json.toJSON t
                  , "args" .= Json.toJSON ts
                  ]

              Record fields ext ->
                  [ "tag" .= ("record" :: Text.Text)
                  , "fields" .= Json.toJSON (map (Json.toJSON . second Json.toJSON) fields)
                  , "extension" .= Json.toJSON ext
                  ]

instance Json.FromJSON Type where
    parseJSON (Json.Object obj) =
        do  tag <- obj .: "tag"
            case (tag :: String) of
              "lambda" ->
                  Lambda <$> obj .: "in" <*> obj .: "out"

              "var" ->
                  Var <$> obj .: "name"

              "type" ->
                  Type <$> obj .: "name"

              "app" ->
                  App <$> obj .: "func" <*> obj .: "args"

              "record" ->
                  Record <$> obj .: "fields" <*> obj .: "extension"

              _ ->
                  fail $ "Error when decoding type with tag: " ++ tag


    parseJSON value =
        fail $ "Cannot decode Value from: " ++ BS.unpack (Json.encode value)
