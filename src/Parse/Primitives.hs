{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, Rank2Types, UnboxedTuples, OverloadedStrings #-}
module Parse.Primitives
  ( Parser
  , run, runAt
  , try, deadend, hint, endOfFile
  , oneOf
  , symbol, underscore, keyword, keywords
  , lowVar, capVar, infixOp
  , getPosition, getCol
  , getContext, pushContext, popContext
  , getIndent, setIndent
  , SPos(..), whitespace
  , docComment, chompUntilDocs
  , string, character
  , digit, number
  , shaderSource, shaderFailure
  )
  where

import Prelude hiding (length)
import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad
import qualified Data.Char as Char
import Data.Monoid ((<>))
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
import qualified Reporting.Region as R
import Reporting.Error.Syntax
  ( ParseError(..), Problem(..), Theory(..), BadOp(..)
  )



-- PARSER


newtype Parser a =
  Parser
    { _run
        :: forall b.
           State
        -> (a -> State -> ParseError -> b)  -- consumed ok
        -> (              ParseError -> b)  -- consumed err
        -> (a -> State -> ParseError -> b)  -- empty ok
        -> (              ParseError -> b)  -- empty err
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
    , _context :: E.ContextStack
    }


{-# NOINLINE noError #-}
noError :: ParseError
noError =
  ParseError 0 0 (Theories [] [])


{-# INLINE expect #-}
expect :: Int -> Int -> E.ContextStack -> Theory -> ParseError
expect row col ctx theory =
  ParseError row col (Theories ctx [theory])



-- RUN PARSER


run :: Parser a -> Text -> Either (A.Located E.Error) a
run parser text =
  runAt 1 1 parser text


runAt :: Int -> Int -> Parser a -> Text -> Either (A.Located E.Error) a
runAt sRow sCol parser (Text.Text array offset length) =
  case _run parser (State array offset length 0 sRow sCol []) Ok Err Ok Err of
    Ok value _ _ ->
      Right value

    Err (ParseError row col problem) ->
      let
        pos = R.Position row col
        region = R.Region pos pos
        mkError overallRegion subRegion =
          Left (A.A overallRegion (E.Parse subRegion problem))
      in
        case problem of
          BadOp _ ((_, start) : _) ->
            mkError (R.Region start pos) (Just region)

          Theories ((_, start) : _) _ ->
            mkError (R.Region start pos) (Just region)

          _ ->
            mkError region Nothing


data Result a
  = Ok a State ParseError
  | Err ParseError



-- COMBINATORS


try :: Parser a -> Parser a
try parser =
  Parser $ \state cok _ eok eerr ->
    _run parser state cok eerr eok eerr


shaderFailure :: Int -> Int -> Text -> Parser a
shaderFailure row col msg =
  Parser $ \_ _ cerr _ _ ->
    cerr (ParseError row col (BadShader msg))


deadend :: [Theory] -> Parser a
deadend thrys =
  Parser $ \(State _ _ _ _ row col ctx) _ _ _ eerr ->
    eerr (ParseError row col (Theories ctx thrys))


hint :: E.Next -> Parser a -> Parser a
hint next parser =
  Parser $ \state@(State _ _ _ _ row col ctx) cok cerr eok eerr ->
    let
      eok' x s _ =
        eok x s (expect row col ctx (Expecting next))

      eerr' _ =
        eerr (expect row col ctx (Expecting next))
    in
      _run parser state cok cerr eok' eerr'


endOfFile :: Parser ()
endOfFile =
  Parser $ \state@(State _ _ length _ _ _ _) _ _ eok eerr ->
    if length == 0 then
      eok () state noError

    else
      eerr noError



-- FUNCTOR


instance Functor Parser where
  fmap f parser =
    Parser $ \state cok cerr eok eerr ->
      let
        cok' x s e = cok (f x) s e
        eok' x s e = eok (f x) s e
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
  Parser $ \_ _ _ _ eerr ->
    eerr noError


oneOfHelp :: Parser a -> Parser a -> Parser a
oneOfHelp parser1 parser2 =
  Parser $ \state cok cerr eok eerr ->
    let
      eerr1 e1 =
        let
          eok2 y s e2 = eok y s (mergeErrors e1 e2)
          eerr2 e2 = eerr (mergeErrors e1 e2)
        in
          _run parser2 state cok cerr eok2 eerr2
    in
      _run parser1 state cok cerr eok eerr1


mergeErrors :: ParseError -> ParseError -> ParseError
mergeErrors e1@(ParseError r1 c1 p1) e2@(ParseError r2 c2 p2) =
  case compare r1 r2 of
    LT -> e2
    GT -> e1
    EQ ->
      case compare c1 c2 of
        LT -> e2
        GT -> e1
        EQ ->
          case (p1, p2) of
            (Theories _ [], Theories _ _) ->
              e2

            (Theories _ _, Theories _ []) ->
              e1

            (Theories ctx ts1, Theories _ ts2) ->
              ParseError r1 c1 (Theories ctx (ts1 ++ ts2))

            (Theories _ _, _) ->
              e2

            (_, _) ->
              e1



-- MONAD


instance Monad Parser where
  return value =
    Parser $ \state _ _ eok _ ->
      eok value state noError

  parser >>= callback =
    Parser $ \state cok cerr eok eerr ->
      let
        cok1 x s1 e1 =
          let
            eok2 y s2 e2 = cok y s2 (mergeErrors e1 e2)
            eerr2 e2 = cerr (mergeErrors e1 e2)
          in
            _run (callback x) s1 cok cerr eok2 eerr2

        eok1 x s1 e1 =
          let
            eok2 y s2 e2 = eok y s2 (mergeErrors e1 e2)
            eerr2 e2 = eerr (mergeErrors e1 e2)
          in
            _run (callback x) s1 cok cerr eok2 eerr2
      in
        _run parser state cok1 cerr eok1 eerr



-- TEXT


symbol :: Text -> Parser ()
symbol sym@(Text.Text tArray tOffset tLen) =
  Parser $ \(State array offset length indent row col ctx) cok _ _ eerr ->
    if tLen <= length && Text.equal tArray tOffset array offset tLen then
      let
        (# newRow, newCol #) =
          moveCursor tArray tOffset tLen row col

        !newState =
          State array (offset + tLen) (length - tLen) indent newRow newCol ctx
      in
        cok () newState noError

    else
      eerr (expect row col ctx (Symbol sym))


moveCursor :: Text.Array -> Int -> Int -> Int -> Int -> (# Int, Int #)
moveCursor array offset length row col =
  if length == 0 then
    (# row, col #)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x000A {- \n -} then
        moveCursor array (offset + 1) (length - 1) (row + 1) 1

      else if word < 0xD800 || word > 0xDBFF then
        moveCursor array (offset + 1) (length - 1) row (col + 1)

      else
        moveCursor array (offset + 2) (length - 2) row (col + 1)


underscore :: Parser ()
underscore =
  Parser $ \(State array offset length indent row col ctx) cok cerr _ eerr ->
    if length == 0 || Text.unsafeIndex array offset /= 0x005F {- _ -} then
      eerr noError

    else if length >= 2 && isInnerVarChar (peekChar array (offset + 1)) then
      cerr (ParseError row (col + 1) (BadUnderscore (peekChar array (offset + 1))))

    else
      let
        !newState =
          State array (offset + 1) (length - 1) indent row (col + 1) ctx
      in
        cok () newState noError



-- KEYWORDS


keyword :: Text -> Parser ()
keyword kwd@(Text.Text tArray tOffset tLen) =
  Parser $ \(State array offset length indent row col ctx) cok _ _ eerr ->
    if tLen <= length && Text.equal tArray tOffset array offset tLen then

      if isInnerVarChar (peekChar array (offset + tLen)) then
        eerr (expect row col ctx (Keyword kwd))

      else
        let
          (# newRow, newCol #) =
            moveCursor tArray tOffset tLen row col

          !newState =
            State array (offset + tLen) (length - tLen) indent newRow newCol ctx
        in
          cok () newState noError

    else
      eerr (expect row col ctx (Keyword kwd))


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
  varPrim LowVar Char.isLower


capVar :: Parser Text.Text
capVar =
  varPrim CapVar Char.isUpper


{-# INLINE varPrim #-}
varPrim :: Theory -> (Char -> Bool) -> Parser Text.Text
varPrim theory isGoodFirstChar =
  Parser $ \(State array offset length indent row col ctx) cok _ _ eerr ->
    if length == 0 then
      eerr (expect row col ctx theory)

    else
      let
        (Text.Iter char size) = peek array offset
      in
        if isGoodFirstChar char then
          let
            (# newOffset, newLength, newCol #) =
              varPrimHelp array (offset + size) (length - size) (col + 1)

            copiedText =
              copyText array offset (newOffset - offset)
          in
            if Set.member copiedText keywords then
              eerr (expect row newCol ctx theory)
            else
              cok copiedText (State array newOffset newLength indent row newCol ctx) noError

        else
          eerr (expect row col ctx theory)


{-# INLINE copyText #-}
copyText :: Text.Array -> Int -> Int -> Text.Text
copyText array offset size =
  let
    makeArray =
      do  mutableArray <- Text.new size
          Text.copyI mutableArray 0 array offset size
          return mutableArray
  in
    Text.Text (Text.run makeArray) 0 size


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
  Parser $ \(State array offset length indent row col ctx) cok cerr _ eerr ->
    if length == 0 then
      eerr (expect row col ctx InfixOp)

    else
      let
        (Text.Iter char size) = peek array offset
      in
        if not (Help.isSymbol char) then
          eerr (expect row col ctx InfixOp)
        else
          case infixOpHelp array (offset + size) (length - size) row (col + 1) of
            Left err ->
              cerr err

            Right ( newOffset, newLength, newCol ) ->
              case copyText array offset (newOffset - offset) of
                "."  -> cerr (ParseError row col (BadOp Dot ctx))
                "|"  -> cerr (ParseError row col (BadOp Pipe ctx))
                "->" -> cerr (ParseError row col (BadOp Arrow ctx))
                "="  -> cerr (ParseError row col (BadOp Equals ctx))
                ":"  -> cerr (ParseError row col (BadOp HasType ctx))
                op   -> cok op (State array newOffset newLength indent row newCol ctx) noError


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


getPosition :: Parser R.Position
getPosition =
  Parser $ \state@(State _ _ _ _ row col _) _ _ eok _ ->
    eok (R.Position row col) state noError


getIndent :: Parser Int
getIndent =
  Parser $ \state@(State _ _ _ indent _ _ _) _ _ eok _ ->
    eok indent state noError


getCol :: Parser Int
getCol =
  Parser $ \state@(State _ _ _ _ _ col _) _ _ eok _ ->
    eok col state noError


getContext :: Parser E.ContextStack
getContext =
  Parser $ \state@(State _ _ _ _ _ _ ctx) _ _ eok _ ->
    eok ctx state noError


pushContext :: R.Position -> E.Context -> Parser ()
pushContext pos ctx =
  Parser $ \state@(State _ _ _ _ _ _ context) _ _ eok _ ->
    eok () (state { _context = (ctx, pos) : context }) noError


popContext :: a -> Parser a
popContext value =
  Parser $ \state@(State _ _ _ _ _ _ context) _ _ eok _ ->
    eok value (state { _context = tail context }) noError


setIndent :: Int -> Parser ()
setIndent indent =
  Parser $ \state _ _ eok _ ->
    eok () (state { _indent = indent }) noError



-- WHITESPACE


newtype SPos =
  SPos R.Position


whitespace :: Parser SPos
whitespace =
  Parser $ \(State array offset length indent row col ctx) cok cerr _ _ ->
    case eatSpaces array offset length row col of
      Left err ->
        cerr err

      Right (newOffset, newLength, newRow, newCol) ->
        cok
          (SPos (R.Position newRow newCol))
          (State array newOffset newLength indent newRow newCol ctx)
          noError


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
      Parser $ \(State array offset length indent row col ctx) cok cerr _ _ ->
        case eatMultiCommentHelp array offset length row col 1 of
          Left err ->
            cerr err

          Right (newOffset, newLength, newRow, newCol) ->
            cok
              (copyText array offset (newOffset - offset))
              (State array newOffset newLength indent newRow newCol ctx)
              noError


chompUntilDocs :: Parser Bool
chompUntilDocs =
  Parser $ \(State array offset length indent row col ctx) cok _ _ _ ->
    let
      (# isStart, newOffset, newLength, newRow, newCol #) =
        eatDocs array offset length row col

      !newState =
        State array newOffset newLength indent newRow newCol ctx
    in
      cok isStart newState noError


eatDocs :: Text.Array -> Int -> Int -> Int -> Int -> (# Bool, Int, Int, Int, Int #)
eatDocs array offset length row col =
  if length == 0 then
    (# False, offset, length, row, col #)

  else if isDocsStart array offset length then
    (# True, offset + 5, length - 5, row, col + 5 #)

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x000A {- \n -} then
        eatDocs array (offset + 1) (length - 1) (row + 1) 1

      else if word < 0xD800 || word > 0xDBFF then
        eatDocs array (offset + 1) (length - 1) row (col + 1)

      else
        eatDocs array (offset + 2) (length - 2) row (col + 1)


isDocsStart :: Text.Array -> Int -> Int -> Bool
isDocsStart array offset length =
  length >= 5
  && Text.unsafeIndex array offset       == 0x0040 {- @ -}
  && Text.unsafeIndex array (offset + 1) == 0x0064 {- d -}
  && Text.unsafeIndex array (offset + 2) == 0x006F {- o -}
  && Text.unsafeIndex array (offset + 3) == 0x0063 {- c -}
  && Text.unsafeIndex array (offset + 4) == 0x0073 {- s -}



-- STRINGS


string :: Parser Text.Text
string =
  Parser $ \(State array offset length indent row col ctx) cok cerr _ eerr ->
    if length == 0 then
      eerr noError

    else
      let
        !word = Text.unsafeIndex array offset
      in
        if word /= 0x0022 {- " -} then
          eerr noError

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

              Right ( newOffset, newLength, newCol, builder ) ->
                cok
                  (LT.toStrict (LB.toLazyText builder))
                  (State array newOffset newLength indent row newCol ctx)
                  noError


{-# INLINE isQuote #-}
isQuote :: Text.Array -> Int -> Bool
isQuote array offset =
  Text.unsafeIndex array offset == 0x0022 {- " -}



-- SINGLE STRINGS


singleString :: Text.Array -> Int -> Int -> Int -> Int -> Int -> LB.Builder -> Either ParseError ( Int, Int, Int, LB.Builder )
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
        in
          Right ( offset + 1, length - 1, col + 1, finalBuilder )

      else if word == 0x000A {- \n -} then
        Left (ParseError row col NewLineInString)

      else if word == 0x0027 {- ' -} then

        let
          !newOffset = offset + 1
          chunk = Text.Text array initialOffset (offset - initialOffset)
          newBuilder = builder <> LB.fromText chunk <> LB.fromText "\\'"
        in
          singleString array newOffset (length - 1) row (col + 1) newOffset newBuilder

      else if word == 0x005C {- \ -} then
        case eatEscape array (offset + 1) (length - 1) row (col + 1) EndOfFile_String of
          Left err ->
            Left err

          Right size ->
            singleString array (offset + size) (length - size) row (col + size) initialOffset builder

      else if word < 0xD800 || word > 0xDBFF then
        singleString array (offset + 1) (length - 1) row (col + 1) initialOffset builder

      else
        singleString array (offset + 2) (length - 2) row (col + 1) initialOffset builder


eatEscape :: Text.Array -> Int -> Int -> Int -> Int -> Problem -> Either ParseError Int
eatEscape array offset length row col problem =
  if length == 0 then
    Left (ParseError row col problem)

  else
    case Text.unsafeIndex array offset of
      0x0061 {- a -} -> Right 2
      0x0062 {- b -} -> Right 2
      0x0066 {- f -} -> Right 2
      0x006E {- n -} -> Right 2
      0x0072 {- r -} -> Right 2
      0x0074 {- t -} -> Right 2
      0x0076 {- v -} -> Right 2
      0x0022 {- " -} -> Right 2
      0x005C {- \ -} -> Right 2
      0x0027 {- ' -} -> Right 2
      0x0075 {- u -} | length >= 5 && fourHex array offset -> Right 6
      _ ->
        Left (ParseError row col BadEscape)


fourHex :: Text.Array -> Int -> Bool
fourHex array offset =
  isHex (Text.unsafeIndex array (offset + 1))
  && isHex (Text.unsafeIndex array (offset + 2))
  && isHex (Text.unsafeIndex array (offset + 3))
  && isHex (Text.unsafeIndex array (offset + 4))



-- MULTI STRINGS


multiString :: Text.Array -> Int -> Int -> Int -> Int -> Int -> LB.Builder -> Either ParseError ( Int, Int, Int, LB.Builder )
multiString array offset length row col initialOffset builder =
  if length < 3 then
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
          in
            Right ( offset + 3, length - 3, col + 3, finalBuilder )

      else if word == 0x0027 {- ' -} then

        let
          !newOffset = offset + 1
          chunk = Text.Text array initialOffset (offset - initialOffset)
          newBuilder = builder <> LB.fromText chunk <> LB.fromText "\\'"
        in
          multiString array newOffset (length - 1) row (col + 1) newOffset newBuilder

      else if word == 0x000A {- \n -} then

        let
          !newOffset = offset + 1
          chunk = Text.Text array initialOffset (offset - initialOffset)
          newBuilder = builder <> LB.fromText chunk <> LB.fromText "\\n"
        in
          multiString array newOffset (length - 1) (row + 1) 1 newOffset newBuilder

      else if word == 0x005C {- \ -} then

        case eatEscape array (offset + 1) (length - 1) row (col + 1) EndOfFile_MultiString of
          Left err ->
            Left err

          Right size ->
            multiString array (offset + size) (length - size) row (col + size) initialOffset builder

      else if word < 0xD800 || word > 0xDBFF then
        multiString array (offset + 1) (length - 1) row (col + 1) initialOffset builder

      else
        multiString array (offset + 2) (length - 2) row (col + 1) initialOffset builder



-- CHARACTER


character :: Parser Text
character =
  Parser $ \(State array offset length indent row col ctx) cok cerr _ eerr ->
    if length == 0 then
      eerr noError

    else
      if Text.unsafeIndex array offset /= 0x0027 {- ' -} then
        eerr noError

      else if length < 3 then
        cerr (ParseError row col BadChar)

      else
        case characterHelp array (offset + 1) (length - 1) row (col + 1) of
          Left err ->
            cerr err

          Right (endCol, size) ->
            let
              !newOffset = offset + size + 1
              !newLength = length - size - 1
            in
              if newLength == 0 || Text.unsafeIndex array newOffset /= 0x0027 {- ' -} then
                cerr (ParseError row col BadChar)

              else
                cok
                  (copyText array (offset + 1) size)
                  (State array (newOffset + 1) (newLength - 1) indent row endCol ctx)
                  noError


characterHelp :: Text.Array -> Int -> Int -> Int -> Int -> Either ParseError ( Int, Int )
characterHelp array offset length row col =
  let
    !word = Text.unsafeIndex array offset
  in
    if word == 0x0027 {- ' -} ||  word == 0x000A {- \n -} then
      Left (ParseError row col BadChar)

    else if word == 0x005C {- \ -} then
      do  n <- eatEscape array (offset + 1) (length - 1) row (col + 1) BadChar
          return (col + n + 2, n)

    else if word < 0xD800 || word > 0xDBFF then
      Right (col + 3, 1)

    else if length > 2 then
      Right (col + 3, 2)

    else
      Left (ParseError row col BadChar)



-- DIGITS


digit :: Parser Int
digit =
  Parser $ \(State array offset length indent row col ctx) cok _ _ eerr ->
    if length == 0 then
      eerr (expect row col ctx Digit)

    else
      let
        !word = Text.unsafeIndex array offset
      in
        if isDigit word then
          cok
            (fromIntegral (word - 0x0030))
            (State array (offset + 1) (length - 1) indent row (col + 1) ctx)
            noError
        else
          eerr (expect row col ctx Digit)



-- NUMBERS


number :: Parser L.Literal
number =
  Parser $ \(State array offset length indent row col ctx) cok cerr _ eerr ->
    if length == 0 then
      eerr noError

    else
      let
        !word = Text.unsafeIndex array offset
      in
        if not (isDigit word) then
          eerr noError

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
                  cok
                    literal
                    (State array newOffset newLength indent row (col + (newOffset - offset)) ctx)
                    noError


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
  0x0041 {- A -} <= word && word <= 0x0046 {- F -}


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



-- SHADER


shaderSource :: Parser Text
shaderSource =
  do  symbol "[glsl|"
      Parser $ \(State array offset length indent row col ctx) cok cerr _ _ ->
        case eatShader array offset length row col of
          Nothing ->
            cerr (ParseError row col EndOfFile_Shader)

          Just ( shaderEndOffset, newRow, newCol ) ->
            let
              !size = shaderEndOffset - offset
              !newState =
                State array (shaderEndOffset + 2) (length - size - 2) indent newRow newCol ctx
            in
              cok (copyText array offset size) newState noError


eatShader :: Text.Array -> Int -> Int -> Int -> Int -> Maybe (Int, Int, Int)
eatShader array offset length row col =
  if length < 2 then
    Nothing

  else
    let
      !word = Text.unsafeIndex array offset
    in
      if word == 0x007C {- | -} && Text.unsafeIndex array (offset + 1) == 0x005D {- ] -} then
        Just ( offset, row, col + 2 )

      else if word == 0x000A {- \n -} then
        eatShader array (offset + 1) (length - 1) (row + 1) 1

      else if word < 0xD800 || word > 0xDBFF then
        eatShader array (offset + 1) (length - 1) row (col + 1)

      else
        eatShader array (offset + 2) (length - 2) row (col + 1)