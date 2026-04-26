{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, OverloadedStrings,
QuasiQuotes, Rank2Types, UnboxedTuples
#-}
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


import qualified Data.ByteString.Internal as BS
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import GHC.Exts (Int(..), isTrue#)
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Prim
import GHC.Word (Word8(..))

import qualified Json.String as Json
import qualified Parse.Keyword_TH as TH
import qualified Parse.Primitives as P
import Parse.Primitives (Cursor, slide, newline)
import qualified Reporting.Annotation as A



-- RUNNERS


fromByteString :: Decoder x a -> BS.ByteString -> IO (Either (Error x) a)
fromByteString (Decoder decode) src =
  do  result <- P.fromByteString pFile BadEnd src
      case result of
        Right ast    -> decode ast (pure . Right) (pure . Left . DecodeProblem src)
        Left problem -> pure $ Left $ ParseProblem src problem



-- DECODERS


newtype Decoder x a =
  Decoder (forall r. AST -> (a -> IO r) -> (Problem x -> IO r) -> IO r)



-- ERRORS


data Error x
  = DecodeProblem BS.ByteString (Problem x)
  | ParseProblem BS.ByteString ParseError



-- DECODE PROBLEMS


data Problem x
  = Field BS.ByteString (Problem x)
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
  | TObjectWith BS.ByteString
  | TArrayPair Int



-- INSTANCES


instance Functor (Decoder x) where
  {-# INLINE fmap #-}
  fmap func (Decoder kA) =
    Decoder $ \ast ok err ->
      let
        ok' a = ok (func a)
      in
      kA ast ok' err


instance Applicative (Decoder x) where
  {-# INLINE pure #-}
  pure a =
    Decoder $ \_ ok _ ->
      ok a

  {-# INLINE (<*>) #-}
  (<*>) (Decoder kFunc) (Decoder kArg) =
    Decoder $ \ast ok err ->
      let
        okF func =
          let
            okA arg = ok (func arg)
          in
          kArg ast okA err
      in
      kFunc ast okF err


instance Monad (Decoder x) where
  {-# INLINE (>>=) #-}
  (>>=) (Decoder kA) callback =
    Decoder $ \ast ok err ->
      let
        ok' a =
          case callback a of
            Decoder kB -> kB ast ok err
      in
      kA ast ok' err



-- STRINGS


string :: Decoder x Json.String
string =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      String snippet ->
        ok =<< Json.fromSnippet snippet

      _ ->
        err (Expecting region TString)


customString :: P.Parser x a -> (Cursor -> x) -> Decoder x a
customString parser toBadEnd =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      String snippet ->
        do  result <- P.fromSnippet parser toBadEnd snippet
            case result of
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


listHelp :: Decoder x a -> ([a] -> IO r) -> (Problem x -> IO r) -> Int -> [AST] -> [a] -> IO r
listHelp decoder@(Decoder kA) ok err !i asts revs =
  case asts of
    [] ->
      ok (reverse revs)

    ast:asts ->
      let
        ok' value = listHelp decoder ok err (i+1) asts (value:revs)
        err' prob = err (Index i prob)
      in
      kA ast ok' err'



-- NON-EMPTY LISTS


nonEmptyList :: Decoder x a -> x -> Decoder x (NE.List a)
nonEmptyList decoder x =
  do  values <- list decoder
      case values of
        v:vs -> return (NE.List v vs)
        []   -> failure x



-- PAIR


pair :: Decoder x a -> Decoder x b -> Decoder x (a,b)
pair (Decoder kA) (Decoder decodeB) =
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
            kA astA ok0 err0

          _ ->
            err (Expecting region (TArrayPair (length vs)))

      _ ->
        err (Expecting region TArray)



-- OBJECTS


data KeyDecoder x a =
  KeyDecoder (P.Parser x a) (Cursor -> x)


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


pairsHelp :: KeyDecoder x k -> Decoder x a -> ([(k, a)] -> IO r) -> (Problem x -> IO r) -> [(P.Snippet, AST)] -> [(k, a)] -> IO r
pairsHelp keyDecoder@(KeyDecoder keyParser toBadEnd) valueDecoder@(Decoder kA) ok err kvs revs =
  case kvs of
    [] ->
      ok (reverse revs)

    (snippet, ast) : kvs ->
      do  result <- P.fromSnippet keyParser toBadEnd snippet
          case result of
            Left x ->
              err (Failure (snippetToRegion snippet) x)

            Right key ->
              let
                ok' value = pairsHelp keyDecoder valueDecoder ok err kvs ((key,value):revs)
                err' prob =
                  let !(P.Snippet fpc pos end _) = snippet in
                  err (Field (BS.BS (ForeignPtr pos fpc) (I# (minusAddr# end pos))) prob)
              in
              kA ast ok' err'


snippetToRegion :: P.Snippet -> A.Region
snippetToRegion (P.Snippet _ pos end cur) =
  A.Region cur (slide cur (wordToWord64# (int2Word# (minusAddr# end pos))))



-- FIELDS


field :: BS.ByteString -> Decoder x a -> Decoder x a
field key (Decoder kA) =
  Decoder $ \(A.At region ast) ok err ->
    case ast of
      Object kvs ->
        case findField key kvs of
          Just value ->
            let
              err' prob =
                err (Field key prob)
            in
            kA value ok err'

          Nothing ->
            err (Expecting region (TObjectWith key))

      _ ->
        err (Expecting region TObject)


findField :: BS.ByteString -> [(P.Snippet, AST)] -> Maybe AST
findField key pairs =
  case pairs of
    [] ->
      Nothing

    (P.Snippet fpc pos end _, value) : remainingPairs ->
      if key == BS.BS (ForeignPtr pos fpc) (I# (minusAddr# end pos))
      then Just value
      else findField key remainingPairs



-- ONE OF


oneOf :: [Decoder x a] -> Decoder x a
oneOf decoders =
  Decoder $ \ast ok err ->
    case decoders of
      Decoder kA : decoders ->
        let
          err' e =
            oneOfHelp ast ok err decoders e []
        in
        kA ast ok err'

      [] ->
        error "Ran into (Json.Decode.oneOf [])"


oneOfHelp :: AST -> (a -> IO r) -> (Problem x -> IO r) -> [Decoder x a] -> Problem x -> [Problem x] -> IO r
oneOfHelp ast ok err decoders p ps =
  case decoders of
    Decoder kA : decoders ->
      let
        err' p' =
          oneOfHelp ast ok err decoders p' (p:ps)
      in
      kA ast ok err'

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
mapError func (Decoder kA) =
  Decoder $ \ast ok err ->
    let
      err' prob = err (mapErrorHelp func prob)
    in
    kA ast ok err'


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
  = Start Cursor
  | ObjectField Cursor
  | ObjectColon Cursor
  | ObjectEnd Cursor
  | ArrayEnd Cursor
  | StringProblem StringProblem Cursor
  | NoLeadingZeros Cursor
  | NoFloats Cursor
  | BadEnd Cursor

--  PIndex Int ParseError Cursor
--  PField Json.String ParseError Cursor


data StringProblem
  = BadStringEnd
  | BadStringControlChar
  | BadStringEscapeChar
  | BadStringEscapeHex
  | BadStringNotUtf8



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
    , pTrue  Start >> return TRUE
    , pFalse Start >> return FALSE
    , pNull  Start >> return NULL
    ]


pTrue  :: (Cursor -> x) -> P.Parser x ()
pFalse :: (Cursor -> x) -> P.Parser x ()
pNull  :: (Cursor -> x) -> P.Parser x ()

pTrue  = [TH.keyword|true|]
pFalse = [TH.keyword|false|]
pNull  = [TH.keyword|null|]



-- OBJECT


pObject :: Parser AST_
pObject =
  do  P.word1 0x7B#Word8 {- { -} Start
      spaces
      P.oneOf ObjectField
        [ do  entry <- pField
              spaces
              pObjectHelp [entry]
        , do  P.word1 0x7D#Word8 {-}-} ObjectEnd
              return (Object [])
        ]


pObjectHelp :: [(P.Snippet, AST)] -> Parser AST_
pObjectHelp revEntries =
  P.oneOf ObjectEnd
    [
      do  P.word1 0x2C#Word8 {-,-} ObjectEnd
          spaces
          entry <- pField
          spaces
          pObjectHelp (entry:revEntries)
    ,
      do  P.word1 0x7D#Word8 {-}-} ObjectEnd
          return (Object (reverse revEntries))
    ]


pField :: Parser (P.Snippet, AST)
pField =
  do  key <- pString ObjectField
      spaces
      P.word1 0x3A#Word8 {-:-} ObjectColon
      spaces
      value <- pValue
      return (key, value)



-- ARRAY


pArray :: Parser AST_
pArray =
  do  P.word1 0x5B#Word8 {-[-} Start
      spaces
      P.oneOf Start
        [ do  entry <- pValue
              spaces
              pArrayHelp 1 [entry]
        , do  P.word1 0x5D#Word8 {-]-} ArrayEnd
              return (Array [])
        ]


pArrayHelp :: Int -> [AST] -> Parser AST_
pArrayHelp !len revEntries =
  P.oneOf ArrayEnd
    [
      do  P.word1 0x2C#Word8 {-,-} ArrayEnd
          spaces
          entry <- pValue
          spaces
          pArrayHelp (len + 1) (entry:revEntries)
    ,
      do  P.word1 0x5D#Word8 {-]-} ArrayEnd
          return (Array (reverse revEntries))
    ]



-- STRING


pString :: (Cursor -> ParseError) -> Parser P.Snippet
pString start =
  P.Parser $ \fpc (P.State pos end indent cur) cok _ cerr eerr ->
    if P.ltAddr pos end && P.eqIndex pos 0# 0x22#Word8 {-"-} then

      let
        !pos1 = plusAddr# pos 1#
        !cur1 = slide cur 1#Word64

        !(# status, newPos, newCur #) =
          pStringHelp pos1 end cur1
      in
      case status of
        GoodString ->
          let
            !snp = P.Snippet fpc pos1 (plusAddr# newPos (-1#)) cur1
            !newState = P.State newPos end indent newCur
          in
          cok snp newState

        BadString problem ->
          cerr newCur (StringProblem problem)

    else
      eerr cur start


data StringStatus
  = GoodString
  | BadString StringProblem


pStringHelp :: Addr# -> Addr# -> Cursor -> (# StringStatus, Addr#, Cursor #)
pStringHelp pos end cur =
  if P.notLtAddr pos end then
    (# BadString BadStringEnd, pos, cur #)

  else
    case indexWord8OffAddr# pos 0# of
      0x22#Word8 {-"-} ->
        (# GoodString, plusAddr# pos 1#, slide cur 1#Word64 #)

      0x0A#Word8 {-\n-} ->
        (# BadString BadStringEnd, pos, cur #)

      0x5C#Word8 {-\-} ->
        let !pos1 = plusAddr# pos 1# in
        if P.notLtAddr pos1 end then
          (# BadString BadStringEnd, pos1, slide cur 1#Word64 #)
        else
          case indexWord8OffAddr# pos1 0# of
            0x22#Word8 {-"-} -> pStringHelp (plusAddr# pos 2#) end (slide cur 2#Word64)
            0x5C#Word8 {-\-} -> pStringHelp (plusAddr# pos 2#) end (slide cur 2#Word64)
            0x2F#Word8 {-/-} -> pStringHelp (plusAddr# pos 2#) end (slide cur 2#Word64)
            0x62#Word8 {-b-} -> pStringHelp (plusAddr# pos 2#) end (slide cur 2#Word64)
            0x66#Word8 {-f-} -> pStringHelp (plusAddr# pos 2#) end (slide cur 2#Word64)
            0x6E#Word8 {-n-} -> pStringHelp (plusAddr# pos 2#) end (slide cur 2#Word64)
            0x72#Word8 {-r-} -> pStringHelp (plusAddr# pos 2#) end (slide cur 2#Word64)
            0x74#Word8 {-t-} -> pStringHelp (plusAddr# pos 2#) end (slide cur 2#Word64)
            0x75#Word8 {-u-} ->
              let !pos6 = plusAddr# pos 6# in
              if P.leAddr pos6 end
                && isHex (indexWord8OffAddr# pos 2#)
                && isHex (indexWord8OffAddr# pos 3#)
                && isHex (indexWord8OffAddr# pos 4#)
                && isHex (indexWord8OffAddr# pos 5#)
              then
                pStringHelp pos6 end (slide cur 6#Word64)
              else
                (# BadString BadStringEscapeHex, pos, cur #)

            _ ->
              (# BadString BadStringEscapeChar, pos, cur #)

      word ->
        if isTrue# (ltWord8# word 0x20#Word8) then
          (# BadString BadStringControlChar, pos, cur #)
        else
          let !newPos = P.skipUtf8 pos end word in
          if P.eqAddr pos newPos
          then (# BadString BadStringNotUtf8, pos, cur #)
          else pStringHelp newPos end (slide cur 1#Word64)


isHex :: Word8# -> Bool
isHex w =
     isTrue# (0x30#Word8 {-0-} `leWord8#` w) && isTrue# (w `leWord8#` 0x39#Word8 {-9-})
  || isTrue# (0x61#Word8 {-a-} `leWord8#` w) && isTrue# (w `leWord8#` 0x66#Word8 {-f-})
  || isTrue# (0x41#Word8 {-A-} `leWord8#` w) && isTrue# (w `leWord8#` 0x46#Word8 {-F-})



-- SPACES


spaces :: Parser ()
spaces =
  P.Parser $ \_ state@(P.State pos end indent cur) cok eok _ _ ->
    let
      !(# newPos, newCur #) =
        eatSpaces pos end cur
    in
    if P.eqAddr pos newPos then
      eok () state
    else
      let
        !newState =
          P.State newPos end indent newCur
      in
      cok () newState


eatSpaces :: Addr# -> Addr# -> Cursor -> (# Addr#, Cursor #)
eatSpaces pos end cur =
  if P.notLtAddr pos end then
    (# pos, cur #)

  else
    case indexWord8OffAddr# pos 0# of
      0x20#Word8 {-  -} -> eatSpaces (plusAddr# pos 1#) end (slide cur 1#Word64)
      0x09#Word8 {-\t-} -> eatSpaces (plusAddr# pos 1#) end (slide cur 1#Word64)
      0x0A#Word8 {-\n-} -> eatSpaces (plusAddr# pos 1#) end (newline cur)
      0x0D#Word8 {-\r-} -> eatSpaces (plusAddr# pos 1#) end cur
      _ ->
        (# pos, cur #)



-- INTS


pInt :: Parser AST_
pInt =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    if P.notLtAddr pos end then
      eerr cur Start

    else
      let !word = indexWord8OffAddr# pos 0# in
      if not (isDecimalDigit word) then
        eerr cur Start

      else if isTrue# (eqWord8# word 0x30#Word8 {-0-}) then

        let
          !pos1 = plusAddr# pos 1#
          !newState = P.State pos1 end indent (slide cur 1#Word64)
        in
        if P.ltAddr pos1 end then
          let !word1 = indexWord8OffAddr# pos1 0# in
          if isDecimalDigit word1 then
            cerr (slide cur 1#Word64) NoLeadingZeros
          else if isTrue# (eqWord8# word1 0x2E#Word8 {-.-}) then
            cerr (slide cur 1#Word64) NoFloats
          else
            cok (Int 0) newState
        else
          cok (Int 0) newState

      else
        let
          !(# status, n, newPos #) =
            chompInt (plusAddr# pos 1#) end (fromIntegral (W8# word) - 0x30 {-0-})

          !len = wordToWord64# (int2Word# (minusAddr# newPos pos))
        in
        case status of
          GoodInt ->
            let
              !newState =
                P.State newPos end indent (slide cur len)
            in
            cok (Int n) newState

          BadIntEnd ->
            cerr (slide cur len) NoFloats


data IntStatus = GoodInt | BadIntEnd


chompInt :: Addr# -> Addr# -> Int -> (# IntStatus, Int, Addr# #)
chompInt pos end n =
  if P.ltAddr pos end then
    let !word = indexWord8OffAddr# pos 0# in
    if isDecimalDigit word then
      let !m = 10 * n + fromIntegral (W8# word - 0x30 {-0-}) in
      chompInt (plusAddr# pos 1#) end m
    else if isTrue# (eqWord8# word 0x2E#Word8 {-.-}) || isTrue# (eqWord8# word 0x65#Word8 {-e-}) || isTrue# (eqWord8# word 0x45#Word8 {-E-}) then
      (# BadIntEnd, n, pos #)
    else
      (# GoodInt, n, pos #)

  else
    (# GoodInt, n, pos #)


{-# INLINE isDecimalDigit #-}
isDecimalDigit :: Word8# -> Bool
isDecimalDigit w =
  isTrue# (w `leWord8#` 0x39#Word8 {-9-}) && isTrue# (w `geWord8#` 0x30#Word8 {-0-})
