{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, Rank2Types, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives
  ( Parser
  , run
  , try, failure, deadend, expecting, endOfFile
  , oneOf
  , symbol, keyword, keywords
  , lowVar, capVar, infixOp
  , getPosition, getCol
  , getIndent, setIndent
  , SPos(..), whitespace
  , docComment
  , string, character
  , digit, number
  )
  where

import Prelude hiding (length)
import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Array as Text
import qualified Data.Text.Internal as Text
import qualified Data.Text.Unsafe as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LB
import Data.Text.Internal.Encoding.Utf16 (chr2)
import Data.Text.Internal.Unsafe.Char (unsafeChr)
import GHC.Word (Word16(..))

import qualified AST.Helpers as Help (isSymbol)
import qualified AST.Literal as L
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E
import Reporting.Error.Syntax ( ParseError(..), Problem(..), Theory(..) )
import qualified Reporting.Region as Region



-- PARSER


newtype Parser a =
  Parser
    { _run
        :: forall b.
           State
        -> (a -> State -> b)  -- consumed ok
        -> (ParseError -> b)  -- consumed err
        -> (a -> State -> b)  -- empty ok
        -> (ParseError -> b)  -- empty err
        -> b
    }


data State =
  State
    { _array :: !Text.Array
    , _offset :: !Int
    , _length :: !Int
    , _indent :: !Int
    , _row :: !Int
    , _col :: !Int
    }


data Result a
  = Ok a State
  | Err ParseError


{-# INLINE noTheory #-}
noTheory :: Int -> Int -> ParseError
noTheory row col =
  ParseError row col (Theories [])


{-# INLINE loneTheory #-}
loneTheory :: Int -> Int -> Theory -> ParseError
loneTheory row col theory =
  ParseError row col (Theories [theory])



-- RUN PARSER


run :: Parser a -> Text -> Either (A.Located E.Error) a
run parser (Text.Text array offset length) =
  case _run parser (State array offset length 0 1 1) Ok Err Ok Err of
    Ok value _ ->
      Right value

    Err (ParseError row col problem) ->
      let
        region =
          Region.Region (Region.Position row col) (Region.Position row (col + 1))
      in
        Left (A.A region (E.Parse problem))



-- COMBINATORS


try :: Parser a -> Parser a
try parser =
  Parser $ \state cok _ eok eerr ->
    _run parser state cok eerr eok eerr


failure :: Problem -> Parser a
failure problem =
  Parser $ \(State _ _ _ _ row col) _ cerr _ _ ->
    cerr (ParseError row col problem)


deadend :: Theory -> Parser a
deadend theory =
  Parser $ \(State _ _ _ _ row col) _ _ _ eerr ->
    eerr (loneTheory row col theory)


expecting :: Text -> Parser a -> Parser a
expecting description parser =
  Parser $ \state@(State _ _ _ _ row col) cok cerr eok eerr ->
    let
      eok' x s =
        eok x s

      eerr' _ =
        eerr (loneTheory row col (Expecting description))
    in
      _run parser state cok cerr eok' eerr'


endOfFile :: Parser ()
endOfFile =
  Parser $ \state@(State _ _ length _ row col) _ _ eok eerr ->
    if length == 0 then
      eok () state

    else
      eerr (noTheory row col)



-- FUNCTOR


instance Functor Parser where
  fmap f parser =
    Parser $ \state cok cerr eok eerr ->
      let
        cok' x s = cok (f x) s
        eok' x s = eok (f x) s
      in
        _run parser state cok' cerr eok' eerr



-- APPLICATIVE


instance Applicative.Applicative Parser where
    pure = return
    (<*>) = ap


instance Applicative.Alternative Parser where
    empty = allTheOptionsFailed
    (<|>) = oneOfHelp


oneOf :: [Parser a] -> Parser a
oneOf parsers =
  foldr oneOfHelp allTheOptionsFailed parsers


allTheOptionsFailed :: Parser a
allTheOptionsFailed =
  Parser $ \(State _ _ _ _ row col) _ _ _ eerr ->
    eerr (noTheory row col)


oneOfHelp :: Parser a -> Parser a -> Parser a
oneOfHelp parser1 parser2 =
  Parser $ \state cok cerr eok eerr ->
    let
      eerr1 err1 =
        let
          eok2 y state' = eok y state'
          eerr2 err2 = eerr (mergeErrors err1 err2)
        in
          _run parser2 state cok cerr eok2 eerr2
    in
      _run parser1 state cok cerr eok eerr1


mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors error1@(ParseError row col problem1) error2@(ParseError _ _ problem2) =
  case (problem1, problem2) of
    (Theories theories1, Theories theories2) ->
      ParseError row col (Theories (theories1 ++ theories2))

    (Theories _, _) ->
      error2

    (_, _) ->
      error1



-- MONAD


instance Monad Parser where
  return value =
    Parser $ \state _ _ eok _ ->
      eok value state

  parser >>= callback =
    Parser $ \state cok cerr eok eerr ->
      let
        cok' value newState =
          _run (callback value) newState cok cerr cok cerr

        eok' value newState =
          _run (callback value) newState cok cerr eok eerr
      in
        _run parser state cok' cerr eok' eerr



-- TEXT


symbol :: Text -> Parser ()
symbol sym@(Text.Text tArray tOffset tLen) =
  Parser $ \(State array offset length indent row col) cok _ _ eerr ->
    if tLen <= length && Text.equal tArray tOffset array offset tLen then
      let
        (# newRow, newCol #) =
          moveCursor tArray tOffset tLen row col
      in
        cok () (State array (offset + tLen) (length - tLen) indent newRow newCol)

    else
      eerr (loneTheory row col (Symbol sym))


moveCursor :: Text.Array -> Int -> Int -> Int -> Int -> (# Int, Int #)
moveCursor array offset length row col =
  if length == 0 then
    (# row, col #)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x000A then
        moveCursor array (offset + 1) (length - 1) (row + 1) 1

      else if word < 0xD800 || word > 0xDBFF then
        moveCursor array (offset + 1) (length - 1) row (col + 1)

      else
        moveCursor array (offset + 2) (length - 2) row (col + 1)



-- KEYWORDS


keyword :: Text -> Parser ()
keyword kwd@(Text.Text tArray tOffset tLen) =
  Parser $ \(State array offset length indent row col) cok _ _ eerr ->
    if tLen <= length && Text.equal tArray tOffset array offset tLen then

      if isInnerVarChar (peekChar array (offset + tLen)) then
        eerr (loneTheory row col (Keyword kwd))

      else
        let
          (# newRow, newCol #) =
            moveCursor tArray tOffset tLen row col
        in
          cok () (State array (offset + tLen) (length - tLen) indent newRow newCol)

    else
      eerr (loneTheory row col (Keyword kwd))


keywords :: Set.Set Text
keywords =
  Set.fromList
    [ "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "type"
    , "module", "where"
    , "import", "exposing"
    , "as"
    , "port"
    ]



-- VARIABLES


lowVar :: Parser Text.Text
lowVar =
  varPrim Char.isLower


capVar :: Parser Text.Text
capVar =
  varPrim Char.isUpper


{-# INLINE varPrim #-}
varPrim :: (Char -> Bool) -> Parser Text.Text
varPrim isGoodFirstChar =
  Parser $ \(State array offset length indent row col) cok _ _ eerr ->
    if length == 0 then
      eerr (loneTheory row col Variable)

    else
      let
        (Text.Iter char size) = peek array offset
      in
        if isGoodFirstChar char then
          let
            (# newOffset, newLength, newCol #) =
              varPrimHelp array (offset + size) (length - size) (col + 1)

            !newSize =
              newOffset - offset

            makeArray =
              do  mutableArray <- Text.new newSize
                  Text.copyI mutableArray 0 array offset newSize
                  return mutableArray

            copiedText =
              Text.Text (Text.run makeArray) 0 newSize
          in
            if Set.member copiedText keywords then
              eerr (loneTheory row newCol Variable)
            else
              cok copiedText (State array newOffset newLength indent row newCol)

        else
          eerr (loneTheory row col Variable)


{-# INLINE peek #-}
peek :: Text.Array -> Int -> Text.Iter
peek array offset =
  let
    !word = Text.unsafeIndex array offset
  in
    if word < 0xD800 || word > 0xDBFF then
      Text.Iter (unsafeChr word) 1
    else
      Text.Iter (chr2 word (Text.unsafeIndex array (offset + 1))) 2


{-# INLINE peekChar #-}
peekChar :: Text.Array -> Int -> Char
peekChar array offset =
  let
    !word = Text.unsafeIndex array offset
  in
    if word < 0xD800 || word > 0xDBFF then
      unsafeChr word
    else
      chr2 word (Text.unsafeIndex array (offset + 1))


varPrimHelp :: Text.Array -> Int -> Int -> Int -> (# Int, Int, Int #)
varPrimHelp array offset length col =
  if length == 0 then
    (# offset, length, col #)

  else
    let
      (Text.Iter char size) = peek array offset -- TODO look for latin chars directly?
    in
      if isInnerVarChar char then
        varPrimHelp array (offset + size) (length - size) (col + 1)

      else
        (# offset, length, col #)


{-# INLINE isInnerVarChar #-}
isInnerVarChar :: Char -> Bool
isInnerVarChar char =
  Char.isAlphaNum char || char == '_'



-- INFIX OPS


infixOp :: Parser Text
infixOp =
  Parser $ \(State array offset length indent row col) cok cerr _ eerr ->
    if length == 0 then
      eerr (loneTheory row col InfixOp)

    else
      let
        (Text.Iter char size) = peek array offset
      in
        if not (Help.isSymbol char) then
          eerr (loneTheory row col InfixOp)
        else
          case infixOpHelp array (offset + size) (length - size) row (col + 1) of
            Left err ->
              cerr err

            Right ( newOffset, newLength, newCol ) ->
              let
                !newSize =
                  newOffset - offset

                makeArray =
                  do  mutableArray <- Text.new newSize
                      Text.copyI mutableArray 0 array offset newSize
                      return mutableArray
              in
                case Text.Text (Text.run makeArray) 0 newSize of
                  "="  -> cerr (ParseError row col Dot)
                  "|"  -> cerr (ParseError row col Pipe)
                  ":"  -> cerr (ParseError row col Arrow)
                  "."  -> cerr (ParseError row col Equals)
                  "->" -> cerr (ParseError row col HasType)
                  op   -> cok op (State array newOffset newLength indent row newCol)


infixOpHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either ParseError (Int, Int, Int)
infixOpHelp array offset length row col =
  if length == 0 then
    Right ( offset, length, col )

  else
    let
      (Text.Iter char size) = peek array offset
    in
      if Help.isSymbol char then
        infixOpHelp array (offset + size) (length - size) row (col + 1)
      else
        Right ( offset, length, col )



-- STATE


getPosition :: Parser Region.Position
getPosition =
  Parser $ \state@(State _ _ _ _ row col) _ _ eok _ ->
    eok (Region.Position row col) state


getIndent :: Parser Int
getIndent =
  Parser $ \state@(State _ _ _ indent _ _) _ _ eok _ ->
    eok indent state


getCol :: Parser Int
getCol =
  Parser $ \state@(State _ _ _ _ _ col) _ _ eok _ ->
    eok col state


setIndent :: Int -> Parser ()
setIndent indent =
  Parser $ \state _ _ eok _ ->
    eok () (state { _indent = indent })



-- WHITESPACE


newtype SPos =
  SPos Region.Position


whitespace :: Parser SPos
whitespace =
  Parser $ \(State array offset length indent row col) cok cerr _ _ ->
    case eatSpaces array offset length row col of
      Left err ->
        cerr err

      Right (newOffset, newLength, newRow, newCol) ->
        cok
          (SPos (Region.Position newRow newCol))
          (State array newOffset newLength indent newRow newCol)


eatSpaces :: Text.Array -> Int -> Int -> Int -> Int -> Either ParseError ( Int, Int, Int, Int )
eatSpaces array offset length row col =
  if length == 0 then
    Right ( offset, length, row, col )

  else
    case Text.unsafeIndex array offset of
      0x0020 {-   -} ->
        eatSpaces array (offset + 1) (length - 1) row (col + 1)

      0x000A {- \n -} ->
        eatSpaces array (offset + 1) (length - 1) (row + 1) 1

      0x007B {- { -} ->
        eatMultiComment array offset length row col

      0x002D {- - -} ->
        eatLineComment array offset length row col

      0x000D {- \r -} ->
        eatSpaces array (offset + 1) (length - 1) row col

      0x0009 {- \t -} ->
        Left (ParseError row col Tab)

      _ ->
        Right ( offset, length, row, col )



-- LINE COMMENTS


eatLineComment :: Text.Array -> Int -> Int -> Int -> Int -> Either ParseError ( Int, Int, Int, Int )
eatLineComment array offset length row col =
  if length == 1 then
    Right ( offset, length, row, col )

  else
    case Text.unsafeIndex array (offset + 1) of
      0x002D {- - -} ->
        eatLineCommentHelp array (offset + 2) (length - 2) row (col + 2)

      _ ->
        Right ( offset, length, row, col )


eatLineCommentHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either ParseError ( Int, Int, Int, Int )
eatLineCommentHelp array offset length row col =
  if length == 0 then
    Right ( offset, length, row, col )

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x000A {- \n -} then
        eatSpaces array (offset + 1) (length - 1) (row + 1) 1

      else if word < 0xD800 || word > 0xDBFF then
        eatLineCommentHelp array (offset + 1) (length - 1) row (col + 1)

      else
        eatLineCommentHelp array (offset + 2) (length - 2) row (col + 1)



-- MULTI COMMENTS


eatMultiComment :: Text.Array -> Int -> Int -> Int -> Int -> Either ParseError ( Int, Int, Int, Int )
eatMultiComment array offset length row col =
  if length <= 2 then
    Right ( offset, length, row, col )

  else
    let
      !yesDash = Text.unsafeIndex array (offset + 1) == 0x002D {- - -}
      !noBar   = Text.unsafeIndex array (offset + 2) /= 0x007C {- | -}
    in
      if yesDash && noBar then
        do  (newOffset, newLength, newRow, newCol) <-
              eatMultiCommentHelp array (offset + 2) (length - 2) row (col + 2) 1
            eatSpaces array newOffset newLength newRow newCol

      else
        Right ( offset, length, row, col )


eatMultiCommentHelp :: Text.Array -> Int -> Int -> Int -> Int -> Int -> Either ParseError ( Int, Int, Int, Int )
eatMultiCommentHelp array offset length row col openComments =
  if length == 0 then
    Left (ParseError row col EndOfFile_Comment)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x000A {- \n -} then

        eatMultiCommentHelp array (offset + 1) (length - 1) (row + 1) 1 openComments

      else if word == 0x002D {- - -} && length > 1 && Text.unsafeIndex array (offset + 1) == 0x007D {- } -} then

        if openComments == 1 then
          Right ( offset + 2, length - 2, row, col + 2 )
        else
          eatMultiCommentHelp array (offset + 2) (length - 2) row (col + 2) (openComments - 1)

      else if word == 0x007B {- { -} && length > 1 && Text.unsafeIndex array (offset + 1) == 0x002D {- - -} then

        eatMultiCommentHelp array (offset + 2) (length - 2) row (col + 2) (openComments + 1)

      else if word < 0xD800 || word > 0xDBFF then

        eatMultiCommentHelp array (offset + 1) (length - 1) row (col + 1) openComments

      else

        eatMultiCommentHelp array (offset + 2) (length - 2) row (col + 1) openComments



-- DOCUMENTATION COMMENT


docComment :: Parser Text
docComment =
  do  symbol "{-|"
      Parser $ \(State array offset length indent row col) cok cerr _ _ ->
        case eatMultiCommentHelp array offset length row col 1 of
          Left err ->
            cerr err

          Right (newOffset, newLength, newRow, newCol) ->
            let
              !newSize =
                newOffset - offset

              makeArray =
                do  mutableArray <- Text.new newSize
                    Text.copyI mutableArray 0 array offset newSize
                    return mutableArray

              comment =
                Text.Text (Text.run makeArray) 0 newSize
            in
              cok comment (State array newOffset newLength indent newRow newCol)



-- STRINGS


string :: Parser Text.Text
string =
  Parser $ \(State array offset length indent row col) cok cerr _ eerr ->
    if length == 0 then
      eerr (noTheory row col)

    else
      let
        !word = Text.unsafeIndex array offset
      in
        if word /= 0x0022 {- " -} then
          eerr (noTheory row col)

        else
          let
            stringResult =
              if length >= 3 && isQuote array (offset + 1) && isQuote array (offset + 2) then
                multiString array (offset + 3) (length - 3) row (col + 3) (offset + 3) mempty
              else
                singleString array (offset + 1) (length - 1) row (col + 1) (offset + 1) mempty
          in
            case stringResult of
              Left err ->
                cerr err

              Right ( newOffset, newLength, newCol, str ) ->
                cok str (State array newOffset newLength indent row newCol)


{-# INLINE isQuote #-}
isQuote :: Text.Array -> Int -> Bool
isQuote array offset =
  Text.unsafeIndex array offset == 0x0022 {- " -}



-- SINGLE STRINGS


singleString :: Text.Array -> Int -> Int -> Int -> Int -> Int -> LB.Builder -> Either ParseError ( Int, Int, Int, Text.Text )
singleString array offset length row col initialOffset builder =
  if length == 0 then
    Left (ParseError row col EndOfFile_String)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x0022 {- " -} then
        let
          finalBuilder =
            mappend builder $ LB.fromText $
              Text.Text array initialOffset (offset - initialOffset)

          str =
            LT.toStrict (LB.toLazyTextWith (offset - initialOffset) finalBuilder)
        in
          Right ( offset + 1, length - 1, col + 1, str )

      else if word == 0x000A {- \n -} then
        Left (ParseError row col NewLineInString)

      else if word == 0x005C {- \ -} then
        case eatEscape array (offset + 1) (length - 1) row (col + 1) EndOfFile_String of
          Left err ->
            Left err

          Right ( size, char ) ->
            let
              !newOffset = offset + size
              chunk = Text.Text array initialOffset (offset - initialOffset)
              newBuilder = mappend builder (mappend (LB.fromText chunk) (LB.singleton char))
            in
              singleString array newOffset (length - size) row (col + size) newOffset newBuilder

      else if word < 0xD800 || word > 0xDBFF then
        singleString array (offset + 1) (length - 1) row (col + 1) initialOffset builder

      else
        singleString array (offset + 2) (length - 2) row (col + 1) initialOffset builder


eatEscape :: Text.Array -> Int -> Int -> Int -> Int -> Problem -> Either ParseError ( Int, Char )
eatEscape array offset length row col problem =
  if length == 0 then
    Left (ParseError row col problem)

  else
    case Text.unsafeIndex array offset of
      0x0061 {- a -} -> Right ( 2, '\a' )
      0x0062 {- b -} -> Right ( 2, '\b' )
      0x0066 {- f -} -> Right ( 2, '\f' )
      0x006E {- n -} -> Right ( 2, '\n' )
      0x0072 {- r -} -> Right ( 2, '\r' )
      0x0074 {- t -} -> Right ( 2, '\t' )
      0x0076 {- v -} -> Right ( 2, '\v' )
      0x0022 {- " -} -> Right ( 2, '\"' )
      0x005C {- \ -} -> Right ( 2, '\\' )
      0x0027 {- ' -} -> Right ( 2, '\'' )
      0x0078 {- x -} ->
        case eatHex array (offset + 1) (length - 1) 0 of
          Nothing ->
            Left (ParseError row col BadEscape)

          Just (newOffset, code) ->
            if code <= 0x10FFFF then
              Right ( 1 + (newOffset - offset), toEnum (fromInteger code) )
            else
              Left (ParseError row col BadEscape)

      _ ->
        Left (ParseError row col BadEscape)


eatHex :: Text.Array -> Int -> Int -> Integer -> Maybe ( Int, Integer )
eatHex array offset length n =
  if length < 3 then
    Nothing

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if 0x0030 <= word && word <= 0x0039 then
        eatHex array (offset + 1) (length - 1) (16 * n + fromIntegral (word - 0x0030))

      else if 0x0061 <= word && word <= 0x0066 then
        eatHex array (offset + 1) (length - 1) (16 * n + 10 + fromIntegral (word - 0x0061))

      else if 0x0041 <= word && word <= 0x0046 then
        eatHex array (offset + 1) (length - 1) (16 * n + 10 + fromIntegral (word - 0x0041))

      else
        Just (offset, n)



-- MULTI STRINGS


multiString :: Text.Array -> Int -> Int -> Int -> Int -> Int -> LB.Builder -> Either ParseError ( Int, Int, Int, Text.Text )
multiString array offset length row col initialOffset builder =
  if length == 0 then
    Left (ParseError row col EndOfFile_MultiString)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x0022 {- " -} && isQuote array (offset + 1) && isQuote array (offset + 2) then
        let
          finalBuilder =
            mappend builder $ LB.fromText $
              Text.Text array initialOffset (offset - initialOffset)

          str =
            LT.toStrict (LB.toLazyTextWith (offset - initialOffset) finalBuilder)
        in
          Right ( offset + 3, length - 3, col + 3, str )

      else if word == 0x005C {- \ -} then
        case eatEscape array (offset + 1) (length - 1) row (col + 1) EndOfFile_MultiString of
          Left err ->
            Left err

          Right ( size, char ) ->
            let
              !newOffset = offset + size
              chunk = Text.Text array initialOffset (offset - initialOffset)
              newBuilder = mappend builder (mappend (LB.fromText chunk) (LB.singleton char))
            in
              multiString array newOffset (length - size) row (col + size) newOffset newBuilder

      else if word < 0xD800 || word > 0xDBFF then
        multiString array (offset + 1) (length - 1) row (col + 1) initialOffset builder

      else
        multiString array (offset + 2) (length - 2) row (col + 1) initialOffset builder



-- CHARACTER


character :: Parser Char
character =
  Parser $ \(State array offset length indent row col) cok cerr _ eerr ->
    if length == 0 then
      eerr (noTheory row col)

    else
      if Text.unsafeIndex array offset /= 0x0027 {- ' -} then
        eerr (noTheory row col)

      else if length < 3 then
        cerr (ParseError row col BadChar)

      else
        case characterHelp array (offset + 1) (length - 1) row (col + 1) of
          Left err ->
            cerr err

          Right ( size, char ) ->
            let
              !newOffset = offset + size + 1
              !newLength = length - size - 1
            in
              if newLength == 0 then
                cerr (ParseError row col BadChar)

              else if Text.unsafeIndex array newOffset == 0x0027 {- ' -} then
                cok char (State array (newOffset + 1) (newLength - 1) indent row (col + size + 2))

              else
                cerr (ParseError row col BadChar)


characterHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either ParseError ( Int, Char )
characterHelp array offset length row col =
  let
    !word = Text.unsafeIndex array offset
  in
    if word == 0x0027 {- ' -} then
      Left (ParseError row col BadChar)

    else if word == 0x005C {- \ -} then
      eatEscape array (offset + 1) (length - 1) row (col + 1) BadChar

    else if word == 0x000A {- \n -} then
      Left (ParseError row col BadChar)

    else if word < 0xD800 || word > 0xDBFF then
      Right (1, unsafeChr word)

    else if length > 2 then
      Right (2, chr2 word (Text.unsafeIndex array offset))

    else
      Left (ParseError row col BadChar)



-- DIGITS


digit :: Parser Int
digit =
  Parser $ \(State array offset length indent row col) cok _ _ eerr ->
    if length == 0 then
      eerr (loneTheory row col Digit)

    else
      let
        !word = Text.unsafeIndex array offset
      in
        if isDigit word then
          cok
            (fromIntegral (word - 0x0030))
            (State array (offset + 1) (length - 1) indent row (col + 1))
        else
          eerr (loneTheory row col Digit)



-- NUMBERS


number :: Parser L.Literal
number =
  Parser $ \(State array offset length indent row col) cok cerr _ eerr ->
    if length == 0 then
      eerr (noTheory row col)

    else
      let
        !word = Text.unsafeIndex array offset
      in
        if not (isDigit word) then
          eerr (noTheory row col)

        else
          let
            chompResults =
              if word == 0x0030 {- 0 -} then
                chompZero array offset (offset + 1) (length - 1)
              else
                chompInt array offset (offset + 1) (length - 1)
          in
            case chompResults of
              Left (newOffset, problem) ->
                cerr (ParseError row (col + (newOffset - offset)) problem)

              Right (newOffset, newLength, literal) ->
                if isDirtyEnd array newOffset newLength then
                  cerr (ParseError row (col + (newOffset - offset)) BadNumberEnd)

                else
                  cok literal (State array newOffset newLength indent row (col + (newOffset - offset)))


isDirtyEnd :: Text.Array -> Int -> Int -> Bool
isDirtyEnd array offset length =
  if length == 0 then
    False

  else
    let
      !char = peekChar array offset
    in
      Char.isAlpha char || char == '_'


chompInt :: Text.Array -> Int -> Int -> Int -> Either (Int, Problem) (Int, Int, L.Literal)
chompInt array startOffset offset length =
  if length == 0 then

    Right ( offset, length, readInt array startOffset offset )

  else

    let
      !word = Text.unsafeIndex array offset
    in
      if isDigit word then
        chompInt array startOffset (offset + 1) (length - 1)

      else if word == 0x002E {- . -} then
        chompFraction array startOffset (offset + 1) (length - 1)

      else if word == 0x0065 {- e -} || word == 0x0045 {- E -} then
        chompExponent array startOffset (offset + 1) (length - 1)

      else
        Right ( offset, length, readInt array startOffset offset )


chompFraction :: Text.Array -> Int -> Int -> Int -> Either (Int, Problem) (Int, Int, L.Literal)
chompFraction array startOffset offset length =
  if length == 0 then
    Left (offset, BadNumberDot)

  else if isDigit (Text.unsafeIndex array offset) then
    chompFractionHelp array startOffset (offset + 1) (length - 1)

  else
    Left (offset, BadNumberDot)


chompFractionHelp :: Text.Array -> Int -> Int -> Int -> Either (Int, Problem) (Int, Int, L.Literal)
chompFractionHelp array startOffset offset length =
  if length == 0 then
    Right (offset, length, readFloat array startOffset offset)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if isDigit word then
        chompFractionHelp array startOffset (offset + 1) (length - 1)

      else if word == 0x0065 {- e -} || word == 0x0045 {- E -} then
        chompExponent array startOffset (offset + 1) (length - 1)

      else
        Right (offset, length, readFloat array startOffset offset)


chompExponent :: Text.Array -> Int -> Int -> Int -> Either (Int, Problem) (Int, Int, L.Literal)
chompExponent array startOffset offset length =
  if length == 0 then
    Right (offset, length, readFloat array startOffset offset)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if isDigit word then
        chompExponentHelp array startOffset (offset + 1) (length - 1)

      else if word == 0x002B {- + -} || word == 0x002D {- - -} then

        if length > 1 && isDigit (Text.unsafeIndex array (offset + 1)) then
          chompExponentHelp array startOffset (offset + 2) (length - 2)
        else
          Left (offset, BadNumberExp)

      else
        Left (offset, BadNumberExp)


chompExponentHelp :: Text.Array -> Int -> Int -> Int -> Either (Int, Problem) (Int, Int, L.Literal)
chompExponentHelp array startOffset offset length =
  if length == 0 then
    Right (offset, length, readFloat array startOffset offset)

  else if isDigit (Text.unsafeIndex array offset) then
    chompExponentHelp array startOffset (offset + 1) (length - 1)

  else
    Right (offset, length, readFloat array startOffset offset)


chompZero :: Text.Array -> Int -> Int -> Int -> Either (Int, Problem) (Int, Int, L.Literal)
chompZero array startOffset offset length =
  if length == 0 then
    Right ( offset, length, L.IntNum 0 )

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x0078 {- x -} then
        chompHex array startOffset (offset + 1) (length - 1)

      else if word == 0x002E {- . -} then
        chompFraction array startOffset (offset + 1) (length - 1)

      else if isDigit word then
        Left (offset, BadNumberZero)

      else
        Right ( offset, length, L.IntNum 0 )


chompHex :: Text.Array -> Int -> Int -> Int -> Either (Int, Problem) (Int, Int, L.Literal)
chompHex array startOffset offset length =
  if length == 0 then
    Left (offset, BadNumberHex)

  else if isHex (Text.unsafeIndex array offset) then
    chompHexHelp array startOffset (offset + 1) (length - 1)

  else
    Left (offset, BadNumberHex)


chompHexHelp :: Text.Array -> Int -> Int -> Int -> Either (Int, Problem) (Int, Int, L.Literal)
chompHexHelp array startOffset offset length =
  if length == 0 then
    Right ( offset, length, readInt array startOffset offset )

  else if isHex (Text.unsafeIndex array offset) then
    chompHexHelp array startOffset (offset + 1) (length - 1)

  else
    Right ( offset, length, readInt array startOffset offset )



-- NUMBER HELPERS


{-# INLINE isDigit #-}
isDigit :: Word16 -> Bool
isDigit word =
  word <= 0x0039 {- 9 -} && word >= 0x0030 {- 0 -}


{-# INLINE isHex #-}
isHex :: Word16 -> Bool
isHex word =
  word <= 0x0066 {- f -} && (word >= 0x0061 {- a -} || isDigit word || isCapHex word)


{-# INLINE isCapHex #-}
isCapHex :: Word16 -> Bool
isCapHex word =
  word <= 0x0046 {- F -} && word <= 0x0041 {- A -}


{-# INLINE readInt #-}
readInt :: Text.Array -> Int -> Int -> L.Literal
readInt array startOffset endOffset =
  L.IntNum $ read $ Text.unpack $
    Text.Text array startOffset (endOffset - startOffset)


{-# INLINE readFloat #-}
readFloat :: Text.Array -> Int -> Int -> L.Literal
readFloat array startOffset endOffset =
  L.FloatNum $ read $ Text.unpack $
    Text.Text array startOffset (endOffset - startOffset)
