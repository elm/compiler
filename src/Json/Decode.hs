{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.Decode
  ( Decoder, Error(..), decode, parse
  , text, bool, int, float
  , list, dict, maybe
  , field, at
  , map, map2, succeed, fail, andThen
  -- elm specific stuff
  , moduleName, packageName
  )
  where


import Data.Text (Text)
import Prelude hiding (fail, map, maybe)

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Pkg



-- DECODERS


data Decoder a =
  Decoder
    { _run :: (Error -> Error) -> Aeson.Value -> Either Error a
    }


data Error
  = Field Text Error
  | Index Int Error
  | Failure Aeson.Value String


decode :: Decoder a -> Aeson.Value -> Either Error a
decode decoder value =
  _run decoder id value


parse :: Decoder a -> BS.ByteString -> Either (Maybe Error) a
parse decoder bytestring =
  case Aeson.eitherDecode bytestring of
    Left _ ->
      Left Nothing

    Right json ->
      case decode decoder json of
        Left err ->
          Left (Just err)

        Right value ->
          Right value



-- PRIMITIVES


text :: Decoder Text
text =
  Decoder $ \mkError value ->
    case value of
      Aeson.String txt ->
        Right txt

      _ ->
        Left (mkError (Failure value "a string"))


bool :: Decoder Bool
bool =
  Decoder $ \mkError value ->
    case value of
      Aeson.Bool boolean ->
        Right boolean

      _ ->
        Left (mkError (Failure value "a boolean"))


int :: Decoder Int
int =
  Decoder $ \mkError value ->
    case value of
      Aeson.Number number ->
        case Scientific.toBoundedInteger number of
          Nothing ->
            Left (mkError (Failure value "an integer"))

          Just integer ->
            Right integer

      _ ->
        Left (mkError (Failure value "an integer"))


float :: Decoder Float
float =
  Decoder $ \mkError value ->
    case value of
      Aeson.Number number ->
        Right (Scientific.toRealFloat number)

      _ ->
        Left (mkError (Failure value "a float"))



-- DATA STRUCTURES


list :: Decoder a -> Decoder [a]
list (Decoder run) =
  Decoder $ \mkError value ->
    case value of
      Aeson.Array vector ->
        Vector.toList <$>
          Vector.imapM (\i v -> run (mkError . Index i) v) vector

      _ ->
        Left (mkError (Failure value "an array"))


dict :: Decoder a -> Decoder (HashMap.HashMap Text a)
dict (Decoder run) =
  Decoder $ \mkError value ->
    case value of
      Aeson.Object hashMap ->
        HashMap.traverseWithKey (\k v -> run (mkError . Field k) v) hashMap

      _ ->
        Left (mkError (Failure value "an object"))


maybe :: Decoder a -> Decoder (Maybe a)
maybe (Decoder run) =
  Decoder $ \mkError value ->
    case run mkError value of
      Left _ ->
        Right Nothing

      Right a ->
        Right (Just a)



-- OBJECT PRIMITIVES


field :: Text -> Decoder a -> Decoder a
field name (Decoder run) =
  Decoder $ \mkError value ->
    case value of
      Aeson.Object hashMap ->
        case HashMap.lookup name hashMap of
          Just v ->
            run (mkError . Field name) v

          Nothing ->
            Left $ mkError $
              Failure value ("a \"" ++ Text.unpack name ++ "\" field")

      _ ->
        Left (mkError (Failure value "an object"))


at :: [Text] -> Decoder a -> Decoder a
at names decoder =
  foldr field decoder names



-- MAPPING


map :: (a -> value) -> Decoder a -> Decoder value
map func (Decoder run) =
  Decoder $ \mkError value ->
    func <$> run mkError value


map2 :: (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 func (Decoder runA) (Decoder runB) =
  Decoder $ \mkError value ->
    func
      <$> runA mkError value
      <*> runB mkError value


apply :: Decoder (a -> b) -> Decoder a -> Decoder b
apply (Decoder runFunc) (Decoder runArg) =
  Decoder $ \mkError value ->
    do  func <- runFunc mkError value
        arg <- runArg mkError value
        return (func arg)


instance Functor Decoder where
  fmap =
    map


instance Applicative Decoder where
  pure =
    succeed

  (<*>) =
    apply


instance Monad Decoder where
  return =
    succeed

  (>>=) decoder callback =
    andThen callback decoder

  fail msg =
    fail msg



-- FANCY PRIMITIVES


succeed :: a -> Decoder a
succeed a =
  Decoder $ \_ _ -> Right a


fail :: String  -> Decoder a
fail msg =
  Decoder $ \mkError value ->
    Left (mkError (Failure value msg))


andThen :: (a -> Decoder b) -> Decoder a -> Decoder b
andThen callback (Decoder runA) =
  Decoder $ \mkError value ->
    do  a <- runA mkError value
        let (Decoder runB) = callback a
        runB mkError value



-- ELM SPECIFIC STUFF


moduleName :: Decoder Module.Raw
moduleName =
  do  txt <- text
      case Module.nameFromText txt of
        Nothing ->
          fail "a module name like \"Html.Events\""

        Just name ->
          succeed name


packageName :: Decoder Pkg.Name
packageName =
  do  txt <- text
      case Pkg.fromText txt of
        Left _ ->
          fail "a valid project name, like \"elm-lang/core\""

        Right name ->
          succeed name

