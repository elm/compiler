{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.Decode.Internals
  ( Value(..)
  , Decoder(..)
  , Error(..)
  , string, text, bool, int
  , list, dict, pairs
  , maybe
  , field, at
  , index
  , map, map2, mapError
  , succeed, fail
  , andThen, oneOf
  )
  where


import Data.Text (Text)
import Prelude hiding (fail, map, maybe)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector ((!?))



-- VALUE


data Value
  = Array (Vector.Vector Value)
  | Object [(Text, Value)] (HashMap.HashMap Text Value)
  | String Text
  | TRUE
  | FALSE
  | Integer Int
  | NULL



-- DECODERS


newtype Decoder e a =
  Decoder
    { _run :: (Error e -> Error e) -> Value -> Either (Error e) a
    }


data Error e
  = Field Text (Error e)
  | Index Int (Error e)
  | OneOf [Error e]
  | Failure Value e
  | Expecting Value Type


data Type
  = TObject
  | TArray
  | TString
  | TBool
  | TInt
  | TObjectWith Text
  | TArrayWith Int Int



-- PRIMITIVES


string :: Decoder e String
string =
  Text.unpack <$> text


text :: Decoder e Text
text =
  Decoder $ \mkError value ->
    case value of
      String txt ->
        Right txt

      _ ->
        Left (mkError (Expecting value TString))


bool :: Decoder e Bool
bool =
  Decoder $ \mkError value ->
    case value of
      TRUE ->
        Right True

      FALSE ->
        Right False

      _ ->
        Left (mkError (Expecting value TBool))


int :: Decoder e Int
int =
  Decoder $ \mkError value ->
    case value of
      Integer integer ->
        Right integer

      _ ->
        Left (mkError (Expecting value TInt))



-- DATA STRUCTURES


list :: Decoder e a -> Decoder e [a]
list (Decoder run) =
  Decoder $ \mkError value ->
    case value of
      Array vector ->
        Vector.toList <$>
          Vector.imapM (\i v -> run (mkError . Index i) v) vector

      _ ->
        Left (mkError (Expecting value TArray))


dict :: Decoder e a -> Decoder e (HashMap.HashMap Text a)
dict (Decoder run) =
  Decoder $ \mkError value ->
    case value of
      Object _ hashMap ->
        HashMap.traverseWithKey (\k v -> run (mkError . Field k) v) hashMap

      _ ->
        Left (mkError (Expecting value TObject))


pairs :: Decoder e a -> Decoder e [(Text, a)]
pairs (Decoder run) =
  Decoder $ \mkError value ->
    case value of
      Object kvs _ ->
        traverse (\(k,v) -> (,) k <$> run (mkError . Field k) v) kvs

      _ ->
        Left (mkError (Expecting value TObject))


maybe :: Decoder e a -> Decoder e (Maybe a)
maybe (Decoder run) =
  Decoder $ \mkError value ->
    case run mkError value of
      Left _ ->
        Right Nothing

      Right a ->
        Right (Just a)



-- OBJECT PRIMITIVES


field :: Text -> Decoder e a -> Decoder e a
field name (Decoder run) =
  Decoder $ \mkError value ->
    case value of
      Object _ hashMap ->
        case HashMap.lookup name hashMap of
          Just v ->
            run (mkError . Field name) v

          Nothing ->
            Left $ mkError $ Expecting value (ObjectWith name)

      _ ->
        Left (mkError (Expecting value TObject))


at :: [Text] -> Decoder e a -> Decoder e a
at names decoder =
  foldr field decoder names



-- ARRAY PRIMITIVES


index :: Int -> Decoder e a -> Decoder e a
index i (Decoder run) =
  Decoder $ \mkError value ->
    case value of
      Array vector ->
        case vector !? i of
          Just v ->
            run (mkError . Index i) v

          Nothing ->
            Left $ mkError $ Expecting value $ ArrayWith i (Vector.length vector)

      _ ->
        Left (mkError (Expecting value TArray))



-- MAPPING


map :: (a -> value) -> Decoder e a -> Decoder e value
map func (Decoder run) =
  Decoder $ \mkError value ->
    func <$> run mkError value


map2 :: (a -> b -> value) -> Decoder e a -> Decoder e b -> Decoder e value
map2 func (Decoder runA) (Decoder runB) =
  Decoder $ \mkError value ->
    func
      <$> runA mkError value
      <*> runB mkError value


mapError :: (e -> e') -> Decoder e a -> Decoder e' a
mapError func (Decoder run) =
  Decoder $ \mkError value ->
    run (mapErrorHelp func . mkError) value


mapErrorHelp :: (e -> e') -> Error e -> Error e'
mapErrorHelp func err =
  case err of
    Field f subErr -> Field f (mapErrorHelp func subErr)
    Index i subErr -> Index i (mapErrorHelp func subErr)
    OneOf errors   -> OneOf (map (mapErrorHelp func) errors)
    Failure v e    -> Failure v (func e)
    Expecting v e  -> Expecting v e


apply :: Decoder e (a -> b) -> Decoder e a -> Decoder e b
apply (Decoder runFunc) (Decoder runArg) =
  Decoder $ \mkError value ->
    do  func <- runFunc mkError value
        arg <- runArg mkError value
        return (func arg)


instance Functor (Decoder e) where
  fmap =
    map


instance Applicative (Decoder e) where
  pure =
    succeed

  (<*>) =
    apply


instance Monad (Decoder e) where
  return =
    succeed

  (>>=) decoder callback =
    andThen callback decoder



-- FANCY PRIMITIVES


succeed :: a -> Decoder e a
succeed a =
  Decoder $ \_ _ -> Right a


fail :: e -> Decoder e a
fail err =
  Decoder $ \mkError value ->
    Left (mkError (Failure value err))


andThen :: (a -> Decoder e b) -> Decoder e a -> Decoder e b
andThen callback (Decoder runA) =
  Decoder $ \mkError value ->
    do  a <- runA mkError value
        let (Decoder runB) = callback a
        runB mkError value



-- ONE OF


oneOf :: [Decoder e a] -> Decoder e a
oneOf decoders =
  Decoder (oneOfHelp decoders [])


oneOfHelp :: [Decoder e a] -> [Error e] -> (Error e -> Error e) -> Value -> Either (Error e) a
oneOfHelp decoders errors mkError value =
  case decoders of
    [] ->
      Left (mkError (OneOf (reverse errors)))

    Decoder run : otherDecoders ->
      case run mkError value of
        Right a ->
          Right a

        Left err ->
          oneOfHelp otherDecoders (err:errors) mkError value
