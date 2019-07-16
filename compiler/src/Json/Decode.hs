{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, Rank2Types, OverloadedStrings, UnboxedTuples #-}
module Json.Decode
  ( fromByteString
  , Decoder
  , string
  , customString
  , bool
  , int
  , list
  , nonEmptyList
  , pair
  --
  , KeyDecoder(..)
  , dict
  , pairs
  , field
  --
  , oneOf
  , failure
  , mapError
  --
  , Error(..)
  , Problem(..)
  , DecodeExpectation(..)
  , ParseError(..)
  , StringProblem(..)
  )
  where


import qualified Data.ByteString.Internal as B
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified Json.String as Json
import qualified Parse.Keyword as K
import qualified Parse.Primitives as P
import Parse.Primitives (Row, Col)
import qualified Reporting.Annotation as A



-- RUNNERS


fromByteString :: Decoder x a -> B.ByteString -> Either (Error x) a
fromByteString (Decoder decode) src =
  case P.fromByteString pFile BadEnd src of
    Right ast ->
      decode ast Right (Left . DecodeProblem src)

    Left problem ->
      Left (ParseProblem src problem)



-- DECODERS


newtype Decoder x a =
  Decoder
  (
    forall b.
      AST
      -> (a -> b)
      -> (Problem x -> b)
      -> b
  )



-- ERRORS


data Error x
  = DecodeProblem B.ByteString (Problem x)
  | ParseProblem B.ByteString ParseError



-- DECODE PROBLEMS


data Problem x
  = Field B.ByteString (Problem x)
  | Index Int (Problem x)
  | OneOf (Problem x) [Problem x]
  | Failure A.Region x
  | Expecting A.Region DecodeExpectation


data DecodeExpectation
  = TObject
  | TArray
  | TString
  | TBool
  | TInt
  | TObjectWith B.ByteString
  | TArrayPair Int



-- INSTANCES


instance Functor (Decoder x) where
  {-# INLINE fmap #-}
  fmap func (Decoder decodeA) =
    Decoder $ \ast ok err ->
      let
        ok' a = ok (func a)
      in
      decodeA ast ok' err


instance Applicative (Decoder x) where
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


instance Monad (Decoder x) where
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


string :: Decoder x Json.String
string =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      String snippet ->
        ok (Json.fromSnippet snippet)

      _ ->
        err (Expecting region TString)


customString :: P.Parser x a -> (Row -> Col -> x) -> Decoder x a
customString parser toBadEnd =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      String snippet ->
        case P.fromSnippet parser toBadEnd snippet of
          Right a -> ok a
          Left  x -> err (Failure region x)

      _ ->
        err (Expecting region TString)



-- BOOL


bool :: Decoder x Bool
bool =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      TRUE ->
        ok True

      FALSE ->
        ok False

      _ ->
        err (Expecting region TBool)



-- INT


int :: Decoder x Int
int =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      Int n ->
        ok n

      _ ->
        err (Expecting region TInt)



-- LISTS


list :: Decoder x a -> Decoder x [a]
list decoder =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      Array asts ->
        listHelp decoder ok err 0 asts []

      _ ->
        err (Expecting region TArray)


listHelp :: Decoder x a -> ([a] -> b) -> (Problem x -> b) -> Int -> [AST] -> [a] -> b
listHelp decoder@(Decoder decodeA) ok err !i asts revs =
  case asts of
    [] ->
      ok (reverse revs)

    ast:asts ->
      let
        ok' value = listHelp decoder ok err (i+1) asts (value:revs)
        err' prob = err (Index i prob)
      in
      decodeA ast ok' err'



-- NON-EMPTY LISTS


nonEmptyList :: Decoder x a -> x -> Decoder x (NE.List a)
nonEmptyList decoder x =
  do  values <- list decoder
      case values of
        v:vs -> return (NE.List v vs)
        []   -> failure x



-- PAIR


pair :: Decoder x a -> Decoder x b -> Decoder x (a,b)
pair (Decoder decodeA) (Decoder decodeB) =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      Array vs ->
        case vs of
          [astA,astB] ->
            let
              err0 e = err (Index 0 e)
              ok0 a =
                let
                  err1 e = err (Index 1 e)
                  ok1 b = ok (a,b)
                in
                decodeB astB ok1 err1
            in
            decodeA astA ok0 err0

          _ ->
            err (Expecting region (TArrayPair (length vs)))

      _ ->
        err (Expecting region TArray)



-- OBJECTS


data KeyDecoder x a =
  KeyDecoder (P.Parser x a) (Row -> Col -> x)


dict :: (Ord k) => KeyDecoder x k -> Decoder x a -> Decoder x (Map.Map k a)
dict keyDecoder valueDecoder =
  Map.fromList <$> pairs keyDecoder valueDecoder


pairs :: KeyDecoder x k -> Decoder x a -> Decoder x [(k, a)]
pairs keyDecoder valueDecoder =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      Object kvs ->
        pairsHelp keyDecoder valueDecoder ok err kvs []

      _ ->
        err (Expecting region TObject)


pairsHelp :: KeyDecoder x k -> Decoder x a -> ([(k, a)] -> b) -> (Problem x -> b) -> [(P.Snippet, AST)] -> [(k, a)] -> b
pairsHelp keyDecoder@(KeyDecoder keyParser toBadEnd) valueDecoder@(Decoder decodeA) ok err kvs revs =
  case kvs of
    [] ->
      ok (reverse revs)

    (snippet, ast) : kvs ->
      case P.fromSnippet keyParser toBadEnd snippet of
        Left x ->
          err (Failure (snippetToRegion snippet) x)

        Right key ->
          let
            ok' value = pairsHelp keyDecoder valueDecoder ok err kvs ((key,value):revs)
            err' prob =
              let (P.Snippet fptr off len _ _) = snippet in
              err (Field (B.PS fptr off len) prob)
          in
          decodeA ast ok' err'


snippetToRegion :: P.Snippet -> A.Region
snippetToRegion (P.Snippet _ _ len row col) =
  A.Region (A.Position row col) (A.Position row (col + fromIntegral len))



-- FIELDS


field :: B.ByteString -> Decoder x a -> Decoder x a
field key (Decoder decodeA) =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      Object kvs ->
        case findField key kvs of
          Just value ->
            let
              err' prob =
                err (Field key prob)
            in
            decodeA value ok err'

          Nothing ->
            err (Expecting region (TObjectWith key))

      _ ->
        err (Expecting region TObject)


findField :: B.ByteString -> [(P.Snippet, AST)] -> Maybe AST
findField key pairs =
  case pairs of
    [] ->
      Nothing

    (P.Snippet fptr off len _ _, value) : remainingPairs ->
      if key == B.PS fptr off len
      then Just value
      else findField key remainingPairs



-- ONE OF


oneOf :: [Decoder x a] -> Decoder x a
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


oneOfHelp :: AST -> (a -> b) -> (Problem x -> b) -> [Decoder x a] -> Problem x -> [Problem x] -> b
oneOfHelp ast ok err decoders p ps =
  case decoders of
    Decoder decodeA : decoders ->
      let
        err' p' =
          oneOfHelp ast ok err decoders p' (p:ps)
      in
      decodeA ast ok err'

    [] ->
      err (oneOfError [] p ps)


oneOfError :: [Problem x] -> Problem x -> [Problem x] -> Problem x
oneOfError problems prob ps =
  case ps of
    [] ->
      OneOf prob problems

    p:ps ->
      oneOfError (prob:problems) p ps



-- FAILURE


failure :: x -> Decoder x a
failure x =
  Decoder $ \(A.At region _) _ err ->
    err (Failure region x)



-- ERRORS


mapError :: (x -> y) -> Decoder x a -> Decoder y a
mapError func (Decoder decodeA) =
  Decoder $ \ast ok err ->
    let
      err' prob = err (mapErrorHelp func prob)
    in
    decodeA ast ok err'


mapErrorHelp :: (x -> y) -> Problem x -> Problem y
mapErrorHelp func problem =
  case problem of
    Field k p     -> Field k (mapErrorHelp func p)
    Index i p     -> Index i (mapErrorHelp func p)
    OneOf p ps    -> OneOf (mapErrorHelp func p) (map (mapErrorHelp func) ps)
    Failure r x   -> Failure r (func x)
    Expecting r e -> Expecting r e



-- AST


type AST =
  A.Located AST_


data AST_
  = Array [AST]
  | Object [(P.Snippet, AST)]
  | String P.Snippet
  | Int Int
  | TRUE
  | FALSE
  | NULL



-- PARSE


type Parser a =
  P.Parser ParseError a


data ParseError
  = Start Row Col
  | ObjectField Row Col
  | ObjectColon Row Col
  | ObjectEnd Row Col
  | ArrayEnd Row Col
  | StringProblem StringProblem Row Col
  | NoLeadingZeros Row Col
  | NoFloats Row Col
  | BadEnd Row Col

--  PIndex Int ParseError Row Col
--  PField Json.String ParseError Row Col


data StringProblem
  = BadStringEnd
  | BadStringControlChar
  | BadStringEscapeChar
  | BadStringEscapeHex



-- PARSE AST


pFile :: Parser AST
pFile =
  do  spaces
      value <- pValue
      spaces
      return value


pValue :: Parser AST
pValue =
  P.addLocation $
  P.oneOf Start
    [ String <$> pString Start
    , pObject
    , pArray
    , pInt
    , K.k4 0x74 0x72 0x75 0x65      Start >> return TRUE
    , K.k5 0x66 0x61 0x6C 0x73 0x65 Start >> return FALSE
    , K.k4 0x6E 0x75 0x6C 0x6C      Start >> return NULL
    ]



-- OBJECT


pObject :: Parser AST_
pObject =
  do  P.word1 0x7B {- { -} Start
      spaces
      P.oneOf ObjectField
        [ do  entry <- pField
              spaces
              pObjectHelp [entry]
        , do  P.word1 0x7D {-}-} ObjectEnd
              return (Object [])
        ]


pObjectHelp :: [(P.Snippet, AST)] -> Parser AST_
pObjectHelp revEntries =
  P.oneOf ObjectEnd
    [
      do  P.word1 0x2C {-,-} ObjectEnd
          spaces
          entry <- pField
          spaces
          pObjectHelp (entry:revEntries)
    ,
      do  P.word1 0x7D {-}-} ObjectEnd
          return (Object (reverse revEntries))
    ]


pField :: Parser (P.Snippet, AST)
pField =
  do  key <- pString ObjectField
      spaces
      P.word1 0x3A {-:-} ObjectColon
      spaces
      value <- pValue
      return (key, value)



-- ARRAY


pArray :: Parser AST_
pArray =
  do  P.word1 0x5B {-[-} Start
      spaces
      P.oneOf Start
        [ do  entry <- pValue
              spaces
              pArrayHelp 1 [entry]
        , do  P.word1 0x5D {-]-} ArrayEnd
              return (Array [])
        ]


pArrayHelp :: Int -> [AST] -> Parser AST_
pArrayHelp !len revEntries =
  P.oneOf ArrayEnd
    [
      do  P.word1 0x2C {-,-} ArrayEnd
          spaces
          entry <- pValue
          spaces
          pArrayHelp (len + 1) (entry:revEntries)
    ,
      do  P.word1 0x5D {-]-} ArrayEnd
          return (Array (reverse revEntries))
    ]



-- STRING


pString :: (Row -> Col -> ParseError) -> Parser P.Snippet
pString start =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    if pos < end && P.unsafeIndex pos == 0x22 {-"-} then

      let
        !pos1 = plusPtr pos 1
        !col1 = col + 1

        (# status, newPos, newRow, newCol #) =
          pStringHelp pos1 end row col1
      in
      case status of
        GoodString ->
          let
            !off = minusPtr pos1 (unsafeForeignPtrToPtr src)
            !len = minusPtr newPos pos1 - 1
            !snp = P.Snippet src off len row col1
            !newState = P.State src newPos end indent newRow newCol
          in
          cok snp newState

        BadString problem ->
          cerr newRow newCol (StringProblem problem)

    else
      eerr row col start


data StringStatus
  = GoodString
  | BadString StringProblem


pStringHelp :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> (# StringStatus, Ptr Word8, Row, Col #)
pStringHelp pos end row col =
  if pos >= end then
    (# BadString BadStringEnd, pos, row, col #)

  else
    case P.unsafeIndex pos of
      0x22 {-"-} ->
        (# GoodString, plusPtr pos 1, row, col + 1 #)

      0x0A {-\n-} ->
        (# BadString BadStringEnd, pos, row, col #)

      0x5C {-\-} ->
        let !pos1 = plusPtr pos 1 in
        if pos1 >= end then
          (# BadString BadStringEnd, pos1, row + 1, col #)
        else
          case P.unsafeIndex pos1 of
            0x22 {-"-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x5C {-\-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x2F {-/-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x62 {-b-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x66 {-f-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x6E {-n-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x72 {-r-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x74 {-t-} -> pStringHelp (plusPtr pos 2) end row (col + 2)
            0x75 {-u-} ->
              let !pos6 = plusPtr pos 6 in
              if pos6 <= end
                && isHex (P.unsafeIndex (plusPtr pos 2))
                && isHex (P.unsafeIndex (plusPtr pos 3))
                && isHex (P.unsafeIndex (plusPtr pos 4))
                && isHex (P.unsafeIndex (plusPtr pos 5))
              then
                pStringHelp pos6 end row (col + 6)
              else
                (# BadString BadStringEscapeHex, pos, row, col #)

            _ ->
              (# BadString BadStringEscapeChar, pos, row, col #)

      word ->
        if word < 0x20 then
          (# BadString BadStringControlChar, pos, row, col #)
        else
          let !newPos = plusPtr pos (P.getCharWidth word) in
          pStringHelp newPos end row (col + 1)


isHex :: Word8 -> Bool
isHex word =
     0x30 {-0-} <= word && word <= 0x39 {-9-}
  || 0x61 {-a-} <= word && word <= 0x66 {-f-}
  || 0x41 {-A-} <= word && word <= 0x46 {-F-}



-- SPACES


spaces :: Parser ()
spaces =
  P.Parser $ \state@(P.State src pos end indent row col) cok eok _ _ ->
    let
      (# newPos, newRow, newCol #) =
        eatSpaces pos end row col
    in
    if pos == newPos then
      eok () state
    else
      let
        !newState =
          P.State src newPos end indent newRow newCol
      in
      cok () newState


eatSpaces :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> (# Ptr Word8, Row, Col #)
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


pInt :: Parser AST_
pInt =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    if pos >= end then
      eerr row col Start

    else
      let !word = P.unsafeIndex pos in
      if not (isDecimalDigit word) then
        eerr row col Start

      else if word == 0x30 {-0-} then

        let
          !pos1 = plusPtr pos 1
          !newState = P.State src pos1 end indent row (col + 1)
        in
        if pos1 < end then
          let !word1 = P.unsafeIndex pos1 in
          if isDecimalDigit word1 then
            cerr row (col + 1) NoLeadingZeros
          else if word1 == 0x2E {-.-} then
            cerr row (col + 1) NoFloats
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
                P.State src newPos end indent row (col + len)
            in
            cok (Int n) newState

          BadIntEnd ->
            cerr row (col + len) NoFloats


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
