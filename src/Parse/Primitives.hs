{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, Rank2Types, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives
  ( Parser
  , run
  , try, failure, deadend, expecting, endOfFile
  , oneOf
  , text, keyword, keywords
  , lowVar, capVar, infixOp
  , getPosition, getCol
  , getIndent, setIndent
  , Space(..), whitespace
  , docComment
  , string, character
  , digit
  )
  where

import Prelude hiding (length)
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
import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad

import qualified AST.Helpers as Help (isSymbol)
import qualified Reporting.Region as Region



-- PARSER


newtype Parser a =
  Parser
    { _run
        :: forall b.
           State
        -> (a -> State -> b)  -- consumed ok
        -> (Error -> b)       -- consumed err
        -> (a -> State -> b)  -- empty ok
        -> (Error -> b)       -- empty err
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
  | Err Error


data Error
  = Error !Int !Int
  deriving (Show)



-- RUN PARSER


run :: Parser a -> Text -> Either Error a
run parser (Text.Text array offset length) =
  case _run parser (State array offset length 0 1 1) Ok Err Ok Err of
    Ok value _ ->
      Right value

    Err err ->
      Left err



-- COMBINATORS


try :: Parser a -> Parser a
try parser =
  Parser $ \state cok _ eok eerr ->
    _run parser state cok eerr eok eerr


failure :: error -> Parser a
failure _msg =
  Parser $ \(State _ _ _ _ row col) _ cerr _ _ ->
    cerr (Error row col)


deadend :: error -> Parser a
deadend _msg =
  Parser $ \(State _ _ _ _ row col) _ _ _ eerr ->
    eerr (Error row col)


expecting :: String -> Parser a -> Parser a
expecting _msg parser =
  Parser $ \state@(State _ _ _ _ row col) cok cerr eok eerr ->
    let
      eok' x s = eok x s
      eerr' _err = eerr (Error row col)
    in
      _run parser state cok cerr eok' eerr'


endOfFile :: Parser ()
endOfFile =
  Parser $ \state@(State _ _ length _ row col) _ _ eok eerr ->
    if length == 0 then
      eok () state

    else
      eerr (Error row col)



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
    (<*>) = ap -- TODO: Can this be optimized?


instance Applicative.Alternative Parser where
    empty = allTheOptionsFailed
    (<|>) = oneOfHelp


oneOf :: [Parser a] -> Parser a
oneOf parsers =
  foldr oneOfHelp allTheOptionsFailed parsers


allTheOptionsFailed :: Parser a
allTheOptionsFailed =
  Parser $ \_ _ _ _ eerr ->
    eerr $ error "TODO allTheOptionsFailed"


oneOfHelp :: Parser a -> Parser a -> Parser a
oneOfHelp parser1 parser2 =
  Parser $ \state cok cerr eok eerr ->
    let
      meerr err =
        let
          neok y state' = eok y state'
          neerr err' = eerr (error "TODO oneOfHelp" err err')
        in
          _run parser2 state cok cerr neok neerr
    in
      _run parser1 state cok cerr eok meerr



-- MONAD


instance Monad Parser where
    return x =
      Parser $ \state _ _ eok _ ->
        eok x state

    parser >>= callback =
      Parser $ \state cok cerr eok eerr ->
        let
          cok' x state1 =
            let
              peok y state2 = cok y state2
              peerr err = cerr (error "TODO >>= cok" err)
            in
              _run (callback x) state1 cok cerr peok peerr

          eok' x state1 =
            let
              peok y state2 = eok y state2
              peerr err = eerr (error "TODO >>= eok" err)
            in
              _run (callback x) state1 cok cerr peok peerr
        in
          _run parser state cok' cerr eok' eerr



-- TEXT


text :: Text -> Parser ()
text (Text.Text tArray tOffset tLen) =
  Parser $ \(State array offset length indent row col) cok _ _ eerr ->
    if tLen <= length && Text.equal tArray tOffset array offset tLen then
      let
        (# newRow, newCol #) =
          moveCursor tArray tOffset tLen row col
      in
        cok () (State array (offset + tLen) (length - tLen) indent newRow newCol)

    else
      eerr (error "TODO text")


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
keyword (Text.Text tArray tOffset tLen) =
  Parser $ \(State array offset length indent row col) cok _ _ eerr ->
    if tLen <= length && Text.equal tArray tOffset array offset tLen then

      if Char.isAlphaNum (peekChar array (offset + tLen)) then
        eerr (error "TODO keyword 1")
      else
        let
          (# newRow, newCol #) =
            moveCursor tArray tOffset tLen row col
        in
          cok () (State array (offset + tLen) (length - tLen) indent newRow newCol)

    else
      eerr (error "TODO keyword 2")


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
      eerr (error "TODO varPrim")

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
              eerr (Error row newCol)
            else
              cok copiedText (State array newOffset newLength indent row newCol)

        else
          eerr (error "TODO varPrim 2")


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
      if Char.isAlphaNum char || char == '_' then
        varPrimHelp array (offset + size) (length - size) (col + 1)

      else
        (# offset, length, col #)



-- INFIX OPS


infixOp :: Parser Text
infixOp =
  Parser $ \(State array offset length indent row col) cok cerr _ eerr ->
    if length == 0 then
      eerr (Error row col)
    else
      case infixOpHelp array offset length row col of
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
              "=" -> cerr (Error row col) -- "The = operator is reserved for defining variables. Maybe you want == instead? Or maybe you are defining a variable, but there is whitespace before it?"
              "->" -> cerr (Error row col) -- "Arrows are reserved for cases and anonymous functions. Maybe you want > or >= instead?"
              "|" -> cerr (Error row col) -- "Vertical bars are reserved for use in union type declarations. Maybe you want || instead?"
              ":" -> cerr (Error row col) -- "A single colon is for type annotations. Maybe you want :: instead? Or maybe you are defining a type annotation, but there is whitespace before it?"
              "." -> cerr (Error row col) -- "Dots are for record access. They cannot float around on their own!"
              op -> cok op (State array newOffset newLength indent row newCol)


infixOpHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either Error (Int, Int, Int)
infixOpHelp array offset length row col =
  if length == 0 then
    Left (Error row col)

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


data Space
  = None
  | Freshline
  | BeforeIndent
  | Aligned
  | AfterIndent
  deriving (Eq)


whitespace :: Parser Space
whitespace =
  Parser $ \(State array offset length indent row col) cok cerr _ _ ->
    case eatSpaces array offset length row col of
      Left err ->
        cerr err

      Right (newOffset, newLength, newRow, newCol) ->
        let
          !space =
            if newOffset == offset then
              None -- TODO empty success?
            else if newCol == 1 then
              Freshline
            else if newCol > indent then
              AfterIndent
            else if newCol == indent then
              Aligned
            else
              BeforeIndent
        in
          cok space (State array newOffset newLength indent newRow newCol)


eatSpaces :: Text.Array -> Int -> Int -> Int -> Int -> Either Error ( Int, Int, Int, Int )
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
        Left (Error row col)

      _ ->
        Right ( offset, length, row, col )



-- LINE COMMENTS


eatLineComment :: Text.Array -> Int -> Int -> Int -> Int -> Either Error ( Int, Int, Int, Int )
eatLineComment array offset length row col =
  if length == 1 then
    Right ( offset, length, row, col )

  else
    case Text.unsafeIndex array (offset + 1) of
      0x002D {- - -} ->
        eatLineCommentHelp array (offset + 2) (length - 2) row (col + 2)

      _ ->
        Right ( offset, length, row, col )


eatLineCommentHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either Error ( Int, Int, Int, Int )
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


eatMultiComment :: Text.Array -> Int -> Int -> Int -> Int -> Either Error ( Int, Int, Int, Int )
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


eatMultiCommentHelp :: Text.Array -> Int -> Int -> Int -> Int -> Int -> Either Error ( Int, Int, Int, Int )
eatMultiCommentHelp array offset length row col openComments =
  if length == 0 then
    Left (Error row col) -- never closed the comment!

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
  do  text "{-|"
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
      eerr (Error row col)

    else
      let
        !word = Text.unsafeIndex array offset
      in
        if word /= 0x0022 {- " -} then
          eerr (Error row col)

        else if Text.unsafeIndex array (offset + 1) == 0x0022 {- " -} && Text.unsafeIndex array (offset + 2) == 0x0022 {- " -} then
          error "TODO multiString"

        else
          case singleString array (offset + 1) (length - 1) row (col + 1) (offset + 1) mempty of
            Left msg ->
              cerr msg

            Right ( newOffset, newLength, newCol, str ) ->
              cok str (State array newOffset newLength indent row newCol)



-- SINGLE STRINGS


singleString :: Text.Array -> Int -> Int -> Int -> Int -> Int -> LB.Builder -> Either Error ( Int, Int, Int, Text.Text )
singleString array offset length row col initialOffset builder =
  if length == 0 then
    Left (Error row col)

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
        Left (Error row col)

      else if word == 0x005C {- \ -} then
        case eatEscape array (offset + 1) (length - 1) row (col + 1) of
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


eatEscape :: Text.Array -> Int -> Int -> Int -> Int -> Either Error ( Int, Char )
eatEscape array offset length row col =
  if length == 0 then
    Left (Error row col)

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
            Left (Error row col)

          Just (newOffset, code) ->
            if code <= 0x10FFFF then
              Right ( newOffset - offset + 1, toEnum (fromInteger code) )
            else
              Left (Error row col)

      _ ->
        Left (Error row col)


eatHex :: Text.Array -> Int -> Int -> Integer -> Maybe ( Int, Integer )
eatHex array offset length n =
  if length == 0 then
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



-- CHARACTER


character :: Parser Char
character =
  Parser $ \(State array offset length indent row col) cok cerr _ eerr ->
    if length <= 3 then
      eerr (Error row col)

    else
      if Text.unsafeIndex array offset /= 0x0027 {- ' -} then
        eerr (Error row col)

      else
        case characterHelp array (offset + 1) (length - 1) row (col + 1) of
          Left err ->
            cerr err

          Right ( size, char ) ->
            let
              !newOffset = offset + size
              !newLength = length + size
            in
              if newLength == 0 then
                cerr (Error row col)

              else if Text.unsafeIndex array newOffset == 0x0027 {- ' -} then
                cok char (State array newOffset newLength indent row (col + size + 2))

              else
                cerr (Error row col)


characterHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either Error ( Int, Char )
characterHelp array offset length row col =
  let
    !word = Text.unsafeIndex array offset
  in
    if word == 0x0027 {- ' -} then
      Left (Error row col)

    else if word == 0x005C {- \ -} then
      eatEscape array (offset + 1) (length - 1) row (col + 1)

    else if word == 0x000A {- \n -} then
      Left (Error row col)

    else if word < 0xD800 || word > 0xDBFF then
      Right (1, unsafeChr word)

    else if length > 0 then
      Right (2, chr2 word (Text.unsafeIndex array offset))

    else
      Left (Error row col)



-- NUMBERS


digit :: Parser Int
digit =
  Parser $ \(State array offset length indent row col) cok _ _ eerr ->
    if length == 0 then
      eerr (Error row col)

    else
      let
        !word = Text.unsafeIndex array offset
        !isDigit = word <= 0x0039 {- 9 -} && word >= 0x0030 {- 0 -}
      in
        if isDigit then
          cok
            (fromIntegral (word - 0x0030))
            (State array (offset + 1) (length - 1) indent row (col + 1))
        else
          eerr (Error row col)