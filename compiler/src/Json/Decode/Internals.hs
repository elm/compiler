{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.Decode.Internals
  ( Value(..)
  , Decoder(..)
  , Error(..)
  , Type(..)
  , string, text, name, bool, int
  , list, dict, pairs
  , maybe
  , field, at
  , pair
  , map, map2, mapError
  , succeed, fail
  , andThen, oneOf
  )
  where


import Data.Text (Text)
import Prelude hiding (fail, map, maybe)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified Data.Scientific as Sci
import qualified Data.Text as Text
import qualified Json.Encode as E



-- VALUE


data Value
  = Array [Value]
  | Object [(Text, Value)]
  | String Text
  | Integer Int
  | Float Sci.Scientific
  | TRUE
  | FALSE
  | NULL



-- DECODERS


-- TODO switch to CPS if possible
--
newtype Decoder e a =
  Decoder {
    _run :: Value -> Either (Error e) a
  }


data Error e
  = Field Text (Error e)
  | Index Int (Error e)
  | OneOf [Error e]
  | Failure E.Value e
  | Expecting E.Value Type


data Type
  = TObject
  | TArray
  | TString
  | TBool
  | TInt
  | TObjectWith Text
  | TArrayPair Int



-- PRIMITIVES


string :: Decoder e String
string =
  Text.unpack <$> text


text :: Decoder e Text
text =
  Decoder $ \value ->
    case value of
      String txt ->
        Right txt

      _ ->
        Left (expecting value TString)


name :: Decoder e Name.Name
name =
  Name.fromString <$> string


bool :: Decoder e Bool
bool =
  Decoder $ \value ->
    case value of
      TRUE ->
        Right True

      FALSE ->
        Right False

      _ ->
        Left (expecting value TBool)


int :: Decoder e Int
int =
  Decoder $ \value ->
    case value of
      Integer integer ->
        Right integer

      _ ->
        Left (expecting value TInt)



-- DATA STRUCTURES


list :: Decoder e a -> Decoder e [a]
list (Decoder run) =
  Decoder $ \value ->
    case value of
      Array vs ->
        let
          runHelp i v =
            mapLeft (Index i) (run v)
        in
        sequenceA (zipWith runHelp [0..] vs)

      _ ->
        Left (expecting value TArray)


dict :: Decoder e a -> Decoder e (Map.Map Text a)
dict (Decoder run) =
  Decoder $ \value ->
    case value of
      Object kvs ->
        let
          runHelp k v =
            mapLeft (Field k) (run v)
        in
        Map.traverseWithKey runHelp (Map.fromList kvs)

      _ ->
        Left (expecting value TObject)


pairs :: Decoder e a -> Decoder e [(Text, a)]
pairs (Decoder run) =
  Decoder $ \value ->
    case value of
      Object kvs ->
        let
          runHelp (k,v) =
            case run v of
              Right a -> Right (k, a)
              Left e -> Left (Field k e)
        in
        traverse runHelp kvs

      _ ->
        Left (expecting value TObject)


maybe :: Decoder e a -> Decoder e (Maybe a)
maybe (Decoder run) =
  Decoder $ \value ->
    case run value of
      Right a ->
        Right (Just a)

      Left _ ->
        Right Nothing



-- OBJECT PRIMITIVES


field :: Text -> Decoder e a -> Decoder e a
field k (Decoder run) =
  Decoder $ \value ->
    case value of
      Object kvs ->
        case List.lookup k kvs of
          Just v ->
            mapLeft (Field k) (run v)

          Nothing ->
            Left (expecting value (TObjectWith k))

      _ ->
        Left (expecting value TObject)


at :: [Text] -> Decoder e a -> Decoder e a
at names decoder =
  foldr field decoder names



-- ARRAY PRIMITIVES


pair :: Decoder e a -> Decoder e b -> Decoder e (a,b)
pair (Decoder runA) (Decoder runB) =
  Decoder $ \value ->
    case value of
      Array vs ->
        case vs of
          [a,b] ->
            (,)
              <$> mapLeft (Index 0) (runA a)
              <*> mapLeft (Index 1) (runB b)

          _ ->
            Left (expecting value (TArrayPair (length vs)))

      _ ->
        Left (expecting value TArray)



-- ERRORS


mapError :: (e -> e') -> Decoder e a -> Decoder e' a
mapError func (Decoder run) =
  Decoder $ \value ->
    mapLeft (mapErrorHelp func) (run value)


mapErrorHelp :: (e -> e') -> Error e -> Error e'
mapErrorHelp func err =
  case err of
    Field f subErr -> Field f (mapErrorHelp func subErr)
    Index i subErr -> Index i (mapErrorHelp func subErr)
    OneOf errors   -> OneOf (fmap (mapErrorHelp func) errors)
    Failure v e    -> Failure v (func e)
    Expecting v e  -> Expecting v e


mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft func eith =
  case eith of
    Right a -> Right a
    Left e -> Left (func e)


expecting :: Value -> Type -> Error e
expecting value tipe =
  Expecting (toEncodeValue value) tipe


toEncodeValue :: Value -> E.Value
toEncodeValue value =
  case value of
    Array vs   -> E.array (fmap toEncodeValue vs)
    Object kvs -> E.object (fmap (fmap toEncodeValue) kvs)
    String txt -> E.text txt
    Integer n  -> E.int n
    Float n    -> E.number n
    TRUE       -> E.bool True
    FALSE      -> E.bool False
    NULL       -> E.null



-- MAPPING


map :: (a -> value) -> Decoder e a -> Decoder e value
map func (Decoder run) =
  Decoder $ \value ->
    func <$> run value


map2 :: (a -> b -> value) -> Decoder e a -> Decoder e b -> Decoder e value
map2 func (Decoder runA) (Decoder runB) =
  Decoder $ \value ->
    func
      <$> runA value
      <*> runB value


apply :: Decoder e (a -> b) -> Decoder e a -> Decoder e b
apply (Decoder runFunc) (Decoder runArg) =
  Decoder $ \value ->
    runFunc value <*> runArg value



-- FANCY PRIMITIVES


succeed :: a -> Decoder e a
succeed a =
  Decoder $ \_ -> Right a


fail :: e -> Decoder e a
fail err =
  Decoder $ \value ->
    Left (Failure (toEncodeValue value) err)


andThen :: (a -> Decoder e b) -> Decoder e a -> Decoder e b
andThen callback (Decoder runA) =
  Decoder $ \value ->
    do  a <- runA value
        let (Decoder runB) = callback a
        runB value



-- ONE OF


oneOf :: [Decoder e a] -> Decoder e a
oneOf decoders =
  Decoder (oneOfHelp decoders [])


oneOfHelp :: [Decoder e a] -> [Error e] -> Value -> Either (Error e) a
oneOfHelp decoders errors value =
  case decoders of
    [] ->
      Left (OneOf (reverse errors))

    Decoder run : otherDecoders ->
      case run value of
        Right a ->
          Right a

        Left err ->
          oneOfHelp otherDecoders (err:errors) value



-- INSTANCES


instance Functor (Decoder e) where
  fmap = map


instance Applicative (Decoder e) where
  pure = succeed
  (<*>) = apply


instance Monad (Decoder e) where
  return = succeed
  (>>=) decoder callback =
    andThen callback decoder
