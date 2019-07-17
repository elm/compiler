{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, UnboxedTuples, OverloadedStrings #-}
module Parse.Space
  ( Parser
  --
  , chomp
  , chompAndCheckIndent
  --
  , checkIndent
  , checkAligned
  , checkFreshLine
  --
  , docComment
  )
  where


import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)

import qualified AST.Source as Src
import Parse.Primitives (Row, Col)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- SPACE PARSING


type Parser x a =
  P.Parser x (a, A.Position)



-- CHOMP


chomp :: (E.Space -> Row -> Col -> x) -> P.Parser x ()
chomp toError =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr _ ->
    let
      (# status, newPos, newRow, newCol #) = eatSpaces pos end row col
    in
    case status of
      Good ->
        let
          !newState = P.State src newPos end indent newRow newCol
        in
        cok () newState

      HasTab               -> cerr newRow newCol (toError E.HasTab)
      EndlessMultiComment  -> cerr newRow newCol (toError E.EndlessMultiComment)



-- CHECKS -- to be called right after a `chomp`


checkIndent :: A.Position -> (Row -> Col -> x) -> P.Parser x ()
checkIndent (A.Position endRow endCol) toError =
  P.Parser $ \state@(P.State _ _ _ indent _ col) _ eok _ eerr ->
    if col > indent && col > 1
    then eok () state
    else eerr endRow endCol toError


checkAligned :: (Word16 -> Row -> Col -> x) -> P.Parser x ()
checkAligned toError =
  P.Parser $ \state@(P.State _ _ _ indent row col) _ eok _ eerr ->
    if col == indent
    then eok () state
    else eerr row col (toError indent)


checkFreshLine :: (Row -> Col -> x) -> P.Parser x ()
checkFreshLine toError =
  P.Parser $ \state@(P.State _ _ _ _ row col) _ eok _ eerr ->
    if col == 1
    then eok () state
    else eerr row col toError



-- CHOMP AND CHECK


chompAndCheckIndent :: (E.Space -> Row -> Col -> x) -> (Row -> Col -> x) -> P.Parser x ()
chompAndCheckIndent toSpaceError toIndentError =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr _ ->
    let
      (# status, newPos, newRow, newCol #) = eatSpaces pos end row col
    in
    case status of
      Good ->
        if newCol > indent && newCol > 1
        then

          let
            !newState = P.State src newPos end indent newRow newCol
          in
          cok () newState

        else
          cerr row col toIndentError

      HasTab               -> cerr newRow newCol (toSpaceError E.HasTab)
      EndlessMultiComment  -> cerr newRow newCol (toSpaceError E.EndlessMultiComment)



-- EAT SPACES


data Status
  = Good
  | HasTab
  | EndlessMultiComment


eatSpaces :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> (# Status, Ptr Word8, Row, Col #)
eatSpaces pos end row col =
  if pos >= end then
    (# Good, pos, row, col #)

  else
    case P.unsafeIndex pos of
      0x20 {-   -} ->
        eatSpaces (plusPtr pos 1) end row (col + 1)

      0x0A {- \n -} ->
        eatSpaces (plusPtr pos 1) end (row + 1) 1

      0x7B {- { -} ->
        eatMultiComment pos end row col

      0x2D {- - -} ->
        let !pos1 = plusPtr pos 1 in
        if pos1 < end && P.unsafeIndex pos1 == 0x2D {- - -} then
          eatLineComment (plusPtr pos 2) end row (col + 2)
        else
          (# Good, pos, row, col #)

      0x0D {- \r -} ->
        eatSpaces (plusPtr pos 1) end row col

      0x09 {- \t -} ->
        (# HasTab, pos, row, col #)

      _ ->
        (# Good, pos, row, col #)



-- LINE COMMENTS


eatLineComment :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> (# Status, Ptr Word8, Row, Col #)
eatLineComment pos end row col =
  if pos >= end then
    (# Good, pos, row, col #)

  else
    let !word = P.unsafeIndex pos in
    if word == 0x0A {- \n -} then
      eatSpaces (plusPtr pos 1) end (row + 1) 1
    else
      let !newPos = plusPtr pos (P.getCharWidth word) in
      eatLineComment newPos end row (col + 1)



-- MULTI COMMENTS


eatMultiComment :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> (# Status, Ptr Word8, Row, Col #)
eatMultiComment pos end row col =
  let
    !pos1 = plusPtr pos 1
    !pos2 = plusPtr pos 2
  in
  if pos2 >= end then
    (# Good, pos, row, col #)

  else if P.unsafeIndex pos1 == 0x2D {- - -} then

    if P.unsafeIndex pos2 == 0x7C {- | -} then
      (# Good, pos, row, col #)
    else
      let
        (# status, newPos, newRow, newCol #) =
          eatMultiCommentHelp pos2 end row (col + 2) 1
      in
      case status of
        MultiGood    -> eatSpaces newPos end newRow newCol
        MultiTab     -> (# HasTab, newPos, newRow, newCol #)
        MultiEndless -> (# EndlessMultiComment, pos, row, col #)

  else
    (# Good, pos, row, col #)


data MultiStatus
  = MultiGood
  | MultiTab
  | MultiEndless


eatMultiCommentHelp :: Ptr Word8 -> Ptr Word8 -> Row -> Col -> Word16 -> (# MultiStatus, Ptr Word8, Row, Col #)
eatMultiCommentHelp pos end row col openComments =
  if pos >= end then
    (# MultiEndless, pos, row, col #)

  else
    let !word = P.unsafeIndex pos in
    if word == 0x0A {- \n -} then
      eatMultiCommentHelp (plusPtr pos 1) end (row + 1) 1 openComments

    else if word == 0x09 {- \t -} then
      (# MultiTab, pos, row, col #)

    else if word == 0x2D {- - -} && P.isWord (plusPtr pos 1) end 0x7D {- } -} then
      if openComments == 1 then
        (# MultiGood, plusPtr pos 2, row, col + 2 #)
      else
        eatMultiCommentHelp (plusPtr pos 2) end row (col + 2) (openComments - 1)

    else if word == 0x7B {- { -} && P.isWord (plusPtr pos 1) end 0x2D {- - -} then
      eatMultiCommentHelp (plusPtr pos 2) end row (col + 2) (openComments + 1)

    else
      let !newPos = plusPtr pos (P.getCharWidth word) in
      eatMultiCommentHelp newPos end row (col + 1) openComments



-- DOCUMENTATION COMMENT


docComment :: (Row -> Col -> x) -> (E.Space -> Row -> Col -> x) -> P.Parser x Src.Comment
docComment toExpectation toSpaceError =
  P.Parser $ \(P.State src pos end indent row col) cok _ cerr eerr ->
    let
      !pos3 = plusPtr pos 3
    in
    if pos3 <= end
      && P.unsafeIndex (        pos  ) == 0x7B {- { -}
      && P.unsafeIndex (plusPtr pos 1) == 0x2D {- - -}
      && P.unsafeIndex (plusPtr pos 2) == 0x7C {- | -}
    then
      let
        !col3 = col + 3

        (# status, newPos, newRow, newCol #) =
           eatMultiCommentHelp pos3 end row col3 1
      in
      case status of
        MultiGood ->
          let
            !off = minusPtr pos3 (unsafeForeignPtrToPtr src)
            !len = minusPtr newPos pos3 - 2
            !snippet = P.Snippet src off len row col3
            !comment = Src.Comment snippet
            !newState = P.State src newPos end indent newRow newCol
          in
          cok comment newState

        MultiTab -> cerr newRow newCol (toSpaceError E.HasTab)
        MultiEndless -> cerr row col (toSpaceError E.EndlessMultiComment)
    else
      eerr row col toExpectation
