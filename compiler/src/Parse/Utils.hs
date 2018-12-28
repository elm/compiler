{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns, UnboxedTuples, OverloadedStrings #-}
module Parse.Utils
  ( Parser
  , SParser
  , SPos(..)
  , spaces, noSpace, checkSpace, checkAligned, checkFreshLine
  , whitespace
  , docComment
  )
  where


import Data.Word (Word8, Word16)
import Foreign.Ptr (Ptr, plusPtr)

import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- PARSERS


type Parser a =
  P.Parser E.Context E.Expectation a


type SParser a =
  P.Parser E.Context E.Expectation (a, A.Position, SPos)



-- WHITESPACE VARIATIONS


spaces :: Parser ()
spaces =
  checkSpace =<< whitespace


noSpace :: A.Position -> SPos -> Parser ()
noSpace pos (SPos spos) =
  P.Parser $ \state@(P.State _ _ _ r c context) _ eok _ eerr ->
    if pos == spos
    then eok () state
    else eerr r c context (E.TODO "noSpace")


checkSpace :: SPos -> Parser ()
checkSpace (SPos (A.Position _ col)) =
  P.Parser $ \state@(P.State _ _ indent r c context) _ eok _ eerr ->
    if col > indent && col > 1
    then eok () state
    else eerr r c context (E.TODO "checkSpace")


checkAligned :: SPos -> Parser ()
checkAligned (SPos (A.Position _ col)) =
  P.Parser $ \state@(P.State _ _ indent r c context) _ eok _ eerr ->
    if col == indent
    then eok () state
    else eerr r c context (E.TODO "checkAligned")


checkFreshLine :: SPos -> Parser ()
checkFreshLine (SPos (A.Position _ col)) =
  P.Parser $ \state@(P.State _ _ _ r c context) _ eok _ eerr ->
    if col == 1
    then eok () state
    else eerr r c context (E.TODO "checkFreshLine")



-- WHITESPACE


newtype SPos = SPos A.Position


whitespace :: Parser SPos
whitespace =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
    let
      (# status, newPos, newRow, newCol #) =
        eatSpaces pos end row col
    in
    case status of
      Good ->
        let
          !spos = SPos (A.Position newRow newCol)
          !newState = P.State newPos end indent newRow newCol ctx
        in
        cok spos newState

      HasTab ->
        eerr newRow newCol ctx E.NoTabs

      UnendingMultiComment sr sc ->
        eerr newRow newCol ctx (E.MultiCommentEnd sr sc)


data Status
  = Good
  | HasTab
  | UnendingMultiComment Word16 Word16



-- EAT SPACES


eatSpaces :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> (# Status, Ptr Word8, Word16, Word16 #)
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


eatLineComment :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> (# Status, Ptr Word8, Word16, Word16 #)
eatLineComment pos end row col =
  if pos >= end then
    (# Good, pos, row, col #)

  else
    let !word = P.unsafeIndex pos in
    if word == 0x0A {- \n -} then
      eatSpaces (plusPtr pos 1) end (row + 1) 1
    else
      let !newPos = plusPtr pos (P.getCharWidth pos end word) in
      eatLineComment newPos end row (col + 1)



-- MULTI COMMENTS


eatMultiComment :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> (# Status, Ptr Word8, Word16, Word16 #)
eatMultiComment pos end row col =
  let
    !pos1 = plusPtr pos 1
    !pos2 = plusPtr pos 2
  in
  if pos2 >= end then
    (# Good, pos, row, col #)

  else
    let
      !yesDash = P.unsafeIndex pos1 == 0x2D {- - -}
      !noBar   = P.unsafeIndex pos2 /= 0x7C {- | -}
    in
      if yesDash && noBar then
        let
          (# status, newPos, newRow, newCol #) =
            eatMultiCommentHelp pos2 end row (col + 2) 1 row col
        in
        case status of
          Good ->
            eatSpaces newPos end newRow newCol

          HasTab ->
            (# status, newPos, newRow, newCol #)

          UnendingMultiComment _ _ ->
            (# status, newPos, newRow, newCol #)

      else
        (# Good, pos, row, col #)


eatMultiCommentHelp :: Ptr Word8 -> Ptr Word8 -> Word16 -> Word16 -> Int -> Word16 -> Word16 -> (# Status, Ptr Word8, Word16, Word16 #)
eatMultiCommentHelp pos end row col openComments sr sc =
  if pos >= end then
    (# UnendingMultiComment sr sc, pos, row, col #)

  else
    let !word = P.unsafeIndex pos in
    if word == 0x0A {- \n -} then
      eatMultiCommentHelp (plusPtr pos 1) end (row + 1) 1 openComments sr sc

    else if word == 0x2D {- - -} && P.isWord (plusPtr pos 1) end 0x7D {- } -} then
      if openComments == 1 then
        (# Good, plusPtr pos 2, row, col + 2 #)
      else
        eatMultiCommentHelp (plusPtr pos 2) end row (col + 2) (openComments - 1) sr sc

    else if word == 0x7B {- { -} && P.isWord (plusPtr pos 1) end 0x2D {- - -} then
      eatMultiCommentHelp (plusPtr pos 2) end row (col + 2) (openComments + 1) sr sc

    else
      let !newPos = plusPtr pos (P.getCharWidth pos end word) in
      eatMultiCommentHelp newPos end row (col + 1) openComments sr sc



-- DOCUMENTATION COMMENT


docComment :: Parser ()
docComment =
  P.Parser $ \(P.State pos end indent row col ctx) cok _ _ eerr ->
    let
      !pos3 = plusPtr pos 3
    in
    if pos3 <= end
      && P.unsafeIndex (        pos  ) == 0x7B {- { -}
      && P.unsafeIndex (plusPtr pos 1) == 0x2D {- - -}
      && P.unsafeIndex (plusPtr pos 2) == 0x7C {- | -}
    then
      let
        (# status, newPos, newRow, newCol #) =
           eatMultiCommentHelp pos3 end row (col + 3) 1 row col
      in
      case status of
        Good ->
          let
            !newState = P.State newPos end indent newRow newCol ctx
          in
          cok () newState

        HasTab ->
          eerr newRow newCol ctx E.NoTabs

        UnendingMultiComment sr sc ->
          eerr newRow newCol ctx (E.MultiCommentEnd sr sc)

    else
      eerr row col ctx E.DocComment
