{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, Rank2Types, OverloadedStrings, UnboxedTuples #-}
module Json.Decode
  ( fromByteString
  , Decoder
  , string
  , bool
  , int
  , list, pair
  , dict, pairs, field
  , oneOf
  , failure
  , mapError
  )
  where


import qualified Data.ByteString as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Utf8 as Utf8
import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)

import qualified Parse.Keyword as K
import qualified Parse.Primitives as P
import qualified Reporting.Error.Json as E



-- RUNNERS


fromByteString :: Decoder e a -> B.ByteString -> Either (E.Error e) a
fromByteString decoder src =
  fromAST decoder (P.fromByteString pFile src)


fromAST :: Decoder e a -> P.Result E.ParseError AST -> Either (E.Error e) a
fromAST (Decoder decode) parserResult =
  case parserResult of
    P.Ok ast _ ->
      decode ast Right (Left . E.DecodeProblem)

    P.Err err ->
      Left (E.ParseProblem err)



-- DECODERS


newtype Decoder e a =
  Decoder
  (
    forall b.
      AST
      -> (a -> b)
      -> (E.Problem e -> b)
      -> b
  )



-- INSTANCES


instance Functor (Decoder e) where
  {-# INLINE fmap #-}
  fmap func (Decoder decodeA) =
    Decoder $ \ast ok err ->
      let
        ok' a = ok (func a)
      in
      decodeA ast ok' err


instance Applicative (Decoder e) where
  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) (Decoder decodeFunc) (Decoder decodeArg) =
    Decoder $ \ast ok err ->
      let
        okF func =
          let
            okA arg = ok (func arg)
          in
          decodeArg ast okA err
      in
      decodeFunc ast okF err


instance Monad (Decoder e) where
  {-# INLINE return #-}
  return a =
    Decoder $ \_ ok _ ->
      ok a

  {-# INLINE (>>=) #-}
  (>>=) (Decoder decodeA) callback =
    Decoder $ \ast ok err ->
      let
        ok' a =
          case callback a of
            Decoder decodeB -> decodeB ast ok err
      in
      decodeA ast ok' err



-- STRINGS


string :: Decoder e Utf8.String
string =
  Decoder $ \ast ok err ->
    case ast of
      String bits ->
        ok bits

      _ ->
        err (E.Expecting E.TString)



-- BOOL


bool :: Decoder e Bool
bool =
  Decoder $ \ast ok err ->
    case ast of
      TRUE ->
        ok True

      FALSE ->
        ok False

      _ ->
        err (E.Expecting E.TBool)



-- INT


int :: Decoder e Int
int =
  Decoder $ \ast ok err ->
    case ast of
      Int n ->
        ok n

      _ ->
        err (E.Expecting E.TInt)



-- ARRAYS


list :: Decoder e a -> Decoder e [a]
list decoder =
  Decoder $ \ast ok err ->
    case ast of
      Array asts ->
        listHelp decoder ok err 0 asts []

      _ ->
        err (E.Expecting E.TArray)


listHelp :: Decoder e a -> ([a] -> b) -> (E.Problem e -> b) -> Int -> [AST] -> [a] -> b
listHelp decoder@(Decoder decodeA) ok err !i asts revs =
  case asts of
    [] ->
      ok (reverse revs)

    ast:asts ->
      let
        ok' a = listHelp decoder ok err (i+1) asts (a:revs)
        err' e = err (E.Index i e)
      in
      decodeA ast ok' err'


pair :: Decoder e a -> Decoder e b -> Decoder e (a,b)
pair (Decoder decodeA) (Decoder decodeB) =
  Decoder $ \ast ok err ->
    case ast of
      Array vs ->
        case vs of
          [astA,astB] ->
            let
              err0 e = err (E.Index 0 e)
              ok0 a =
                let
                  err1 e = err (E.Index 1 e)
                  ok1 b = ok (a,b)
                in
                decodeB astB ok1 err1
            in
            decodeA astA ok0 err0

          _ ->
            err (E.Expecting (E.TArrayPair (length vs)))

      _ ->
        err (E.Expecting E.TArray)



-- OBJECT


dict :: Decoder e a -> Decoder e (Map.Map Utf8.String a)
dict decoder =
  Map.fromList <$> pairs decoder


pairs :: Decoder e a -> Decoder e [(Utf8.String, a)]
pairs decoder =
  Decoder $ \ast ok err ->
    case ast of
      Object kvs ->
        pairsHelp decoder ok err kvs []

      _ ->
        err (E.Expecting E.TObject)


pairsHelp :: Decoder e a -> ([(Utf8.String, a)] -> b) -> (E.Problem e -> b) -> [(Utf8.String, AST)] -> [(Utf8.String, a)] -> b
pairsHelp decoder@(Decoder decodeA) ok err kvs revs =
  case kvs of
    [] ->
      ok (reverse revs)

    (key, ast) : kvs ->
      let
        ok' a = pairsHelp decoder ok err kvs ((key,a):revs)
        err' e = err (E.Field key e)
      in
      decodeA ast ok' err'


field :: Utf8.String -> Decoder e a -> Decoder e a
field key (Decoder decodeA) =
  Decoder $ \ast ok err ->
    case ast of
      Object kvs ->
        case List.lookup key kvs of
          Just v ->
            let
              err' e = err (E.Field key e)
            in
            decodeA v ok err'

          Nothing ->
            err (E.Expecting (E.TObjectWith key))

      _ ->
        err (E.Expecting E.TObject)



-- ONE OF


oneOf :: [Decoder e a] -> Decoder e a
oneOf decoders =
  Decoder $ \ast ok err ->
    case decoders of
      Decoder decodeA : decoders ->
        let
          err' e =
            oneOfHelp ast ok err decoders e []
        in
        decodeA ast ok err'

      [] ->
        error "Ran into (Json.Decode.oneOf [])"


oneOfHelp :: AST -> (a -> b) -> (E.Problem e -> b) -> [Decoder e a] -> E.Problem e -> [E.Problem e] -> b
oneOfHelp ast ok err decoders e es =
  case decoders of
    Decoder decodeA : decoders ->
      let
        err' e' =
          oneOfHelp ast ok err decoders e' (e:es)
      in
      decodeA ast ok err'

    [] ->
      err (oneOfError [] e es)


oneOfError :: [E.Problem e] -> E.Problem e -> [E.Problem e] -> E.Problem e
oneOfError errors err es =
  case es of
    [] ->
      E.OneOf err errors

    e:es ->
      oneOfError (err:errors) e es



-- FAILURE


failure :: e -> Decoder e a
failure e =
  Decoder $ \_ _ err ->
    err (E.Failure e)



-- ERRORS


mapError :: (e -> e') -> Decoder e a -> Decoder e' a
mapError func (Decoder decodeA) =
  Decoder $ \ast ok err ->
    let
      err' e = err (mapErrorHelp func e)
    in
    decodeA ast ok err'


mapErrorHelp :: (e -> e') -> E.Problem e -> E.Problem e'
mapErrorHelp func err =
  case err of
    E.Field k e   -> E.Field k (mapErrorHelp func e)
    E.Index i e   -> E.Index i (mapErrorHelp func e)
    E.OneOf e es  -> E.OneOf (mapErrorHelp func e) (map (mapErrorHelp func) es)
    E.Failure e   -> E.Failure (func e)
    E.Expecting e -> E.Expecting e



-- AST


data AST
  = Array [AST]
  | Object [(Utf8.String, AST)]
  | String Utf8.String
  | Int Int
  | TRUE
  | FALSE
  | NULL



-- PARSE


type Parser a =
  P.Parser E.ParseError a


pFile :: Parser AST
pFile =
  do  spaces
      value <- pValue
      spaces
      endOfFile
      return value


pValue :: Parser AST
pValue =
  P.oneOf E.Value
    [ fmap String pString
    , pObject
    , pArray
    , pInt
    , K.k4 0x74 0x72 0x75 0x65      E.Bool >> return TRUE
    , K.k5 0x66 0x61 0x6C 0x73 0x65 E.Bool >> return FALSE
    , K.k4 0x6E 0x75 0x6C 0x6C      E.Null >> return NULL
    ]


endOfFile :: Parser ()
endOfFile =
  P.Parser $ \state@(P.State pos end _ row col) _ eok _ eerr ->
    if pos < end then
      eerr row col E.EndOfFile
    else
      eok () state



-- OBJECT


pObject :: Parser AST
pObject =
  do  P.word1 0x7B {- { -} E.ObjectStart
      spaces
      P.oneOf E.ObjectMore
        [ do  entry <- pField
              spaces
              pObjectHelp [entry]
        , do  P.word1 0x7D {-}-} E.ObjectEnd
              return (Object [])
        ]


pObjectHelp :: [(Utf8.String, AST)] -> Parser AST
pObjectHelp revEntries =
  P.oneOf E.ObjectMore
    [
      do  P.word1 0x2C {-,-} E.ObjectMore
          spaces
          entry <- pField
          spaces
          pObjectHelp (entry:revEntries)
    ,
      do  P.word1 0x7D {-}-} E.ObjectEnd
          return (Object (reverse revEntries))
    ]


pField :: Parser (Utf8.String, AST)
pField =
  do  key <- pString
      spaces
      P.word1 0x3A {-:-} E.Colon
      spaces
      value <- pValue
      return (key, value)



-- ARRAY


pArray :: Parser AST
pArray =
  do  P.word1 0x5B {-[-} E.ArrayStart
      spaces
      P.oneOf E.ArrayMore
        [ do  entry <- pValue
              spaces
              pArrayHelp 1 [entry]
        , do  P.word1 0x5D {-]-} E.ArrayEnd
              return (Array [])
        ]


pArrayHelp :: Int -> [AST] -> Parser AST
pArrayHelp !len revEntries =
  P.oneOf E.ArrayMore
    [
      do  P.word1 0x2C {-,-} E.ArrayMore
          spaces
          entry <- pValue
          spaces
          pArrayHelp (len + 1) (entry:revEntries)
    ,
      do  P.word1 0x5D {-]-} E.ArrayEnd
          return (Array (reverse revEntries))
    ]



-- STRING


pString :: Parser Utf8.String
pString =
  P.Parser $ \(P.State pos end indent row col) cok _ cerr eerr ->
    if pos < end && P.unsafeIndex pos == 0x22 {-"-} then

      let
        !pos1 = plusPtr pos 1

        (# status, newPos, newRow, newCol #) =
          pStringHelp pos1 end row (col + 1)
      in
      case status of
        GoodString ->
          let
            !newState = P.State newPos end indent newRow newCol
            !content = Utf8.fromPtr pos1 (plusPtr newPos (-1))
          in
          cok content newState

        BadStringEnd ->
          cerr newRow newCol E.StringEnd

    else
      eerr row col E.StringStart


data StringStatus
  = GoodString
  | BadStringEnd


pStringHelp :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> (# StringStatus, Ptr Word8, Word16, Word16 #)
pStringHelp pos end row col =
  if pos >= end then
    (# BadStringEnd, pos, row, col #)

  else
    case P.unsafeIndex pos of
      0x22 {-"-} ->
        (# GoodString, plusPtr pos 1, row, col + 1 #)

      0x0A {-\n-} ->
        (# BadStringEnd, pos, row, col #)

      0x5C {-\-} ->
        let
          !pos1 = plusPtr pos 1
        in
        if pos1 < end then

          let
            !word = P.unsafeIndex pos1
            !newPos = plusPtr pos1 (P.getCharWidth pos1 end word)
          in
          pStringHelp newPos end row (col + 2)

        else
          (# BadStringEnd, pos1, row + 1, col #)

      word ->
        let !newPos = plusPtr pos (P.getCharWidth pos end word) in
        pStringHelp newPos end row (col + 1)



-- SPACES


spaces :: Parser ()
spaces =
  P.Parser $ \state@(P.State pos end indent row col) cok eok _ _ ->
    let
      (# newPos, newRow, newCol #) =
        eatSpaces pos end row col
    in
    if pos == newPos then
      eok () state
    else
      let
        !newState =
          P.State newPos end indent newRow newCol
      in
      cok () newState


eatSpaces :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> (# Ptr Word8, Word16, Word16 #)
eatSpaces pos end row col =
  if pos >= end then
    (# pos, row, col #)

  else
    case P.unsafeIndex pos of
      0x20 {-  -} -> eatSpaces (plusPtr pos 1) end row (col + 1)
      0x09 {-\t-} -> eatSpaces (plusPtr pos 1) end row (col + 1)
      0x0A {-\n-} -> eatSpaces (plusPtr pos 1) end (row + 1) 1
      0x0D {-\r-} -> eatSpaces (plusPtr pos 1) end row col
      _ ->
        (# pos, row, col #)



-- INTS


pInt :: Parser AST
pInt =
  P.Parser $ \(P.State pos end indent row col) cok _ cerr eerr ->
    if pos >= end then
      eerr row col E.IntStart

    else
      let !word = P.unsafeIndex pos in
      if not (isDecimalDigit word) then
        eerr row col E.IntStart

      else if word == 0x30 {-0-} then

        let
          !pos1 = plusPtr pos 1
          !newState = P.State pos1 end indent row (col + 1)
        in
        if pos1 < end then
          let !word1 = P.unsafeIndex pos1 in
          if isDecimalDigit word1 then
            cerr row (col + 1) E.NoLeadingZeros
          else if word1 == 0x2E {-.-} then
            cerr row (col + 1) E.NoFloats
          else
            cok (Int 0) newState
        else
          cok (Int 0) newState

      else
        let
          (# status, n, newPos #) =
            chompInt (plusPtr pos 1) end (fromIntegral (word - 0x30 {-0-}))

          !len = fromIntegral (minusPtr newPos pos)
        in
        case status of
          GoodInt ->
            let
              !newState =
                P.State newPos end indent row (col + len)
            in
            cok (Int n) newState

          BadIntEnd ->
            cerr row (col + len) E.NoFloats


data IntStatus = GoodInt | BadIntEnd


chompInt :: Ptr Word8 -> Ptr Word8 -> Int -> (# IntStatus, Int, Ptr Word8 #)
chompInt pos end n =
  if pos < end then
    let !word = P.unsafeIndex pos in
    if isDecimalDigit word then
      let !m = 10 * n + fromIntegral (word - 0x30 {-0-}) in
      chompInt (plusPtr pos 1) end m
    else if word == 0x2E {-.-} || word == 0x65 {-e-} || word == 0x45 {-E-} then
      (# BadIntEnd, n, pos #)
    else
      (# GoodInt, n, pos #)

  else
    (# GoodInt, n, pos #)


{-# INLINE isDecimalDigit #-}
isDecimalDigit :: Word8 -> Bool
isDecimalDigit word =
  word <= 0x39 {-9-} && word >= 0x30 {-0-}
