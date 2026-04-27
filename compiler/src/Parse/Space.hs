{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, UnboxedTuples, UnliftedDatatypes #-}
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


import GHC.Base (UnliftedType)
import GHC.Prim

import qualified AST.Source as Src
import Parse.Primitives (Cursor, Indent, slide, newline, ltAddr, leAddr, notLtAddr, eqIndex)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- SPACE PARSING


type Parser x a =
  P.Parser x (a, A.Position)



-- CHOMP


chomp :: (E.Space -> Cursor -> x) -> P.Parser x ()
chomp toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr _ ->
    let
      !(# status, newPos, newCur #) = eatSpaces pos end cur
    in
    case status of
      Good ->
        let
          !newState = P.State newPos end indent newCur
        in
        cok () newState

      HasTab              -> cerr newCur (toError E.HasTab)
      HasNonUtf8          -> cerr newCur (toError E.HasNonUtf8)
      EndlessMultiComment -> cerr newCur (toError E.EndlessMultiComment)



-- CHECKS -- to be called right after a `chomp`


checkIndent :: A.Position -> (Cursor -> x) -> P.Parser x ()
checkIndent (A.Position endCur) toError =
  P.Parser $ \_ state@(P.State _ _ indent cur) _ eok _ eerr ->
    if P.isIndented indent cur
    then eok () state
    else eerr endCur toError


checkAligned :: (Indent -> Cursor -> x) -> P.Parser x ()
checkAligned toError =
  P.Parser $ \_ state@(P.State _ _ indent cur) _ eok _ eerr ->
    if P.isAligned indent cur
    then eok () state
    else eerr cur (toError indent)


checkFreshLine :: (Cursor -> x) -> P.Parser x ()
checkFreshLine toError =
  P.Parser $ \_ state@(P.State _ _ _ cur) _ eok _ eerr ->
    if P.isLineStart cur
    then eok () state
    else eerr cur toError



-- CHOMP AND CHECK


chompAndCheckIndent :: (E.Space -> Cursor -> x) -> (Cursor -> x) -> P.Parser x ()
chompAndCheckIndent toSpaceError toIndentError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr _ ->
    let
      !(# status, newPos, newCur #) = eatSpaces pos end cur
    in
    case status of
      Good ->
        if P.isIndented indent newCur
        then
          let
            !newState = P.State newPos end indent newCur
          in
          cok () newState

        else
          cerr cur toIndentError

      HasTab              -> cerr newCur (toSpaceError E.HasTab)
      HasNonUtf8          -> cerr newCur (toSpaceError E.HasNonUtf8)
      EndlessMultiComment -> cerr newCur (toSpaceError E.EndlessMultiComment)



-- EAT SPACES


type Status :: UnliftedType
data Status
  = Good
  | HasTab
  | HasNonUtf8
  | EndlessMultiComment


eatSpaces :: Addr# -> Addr# -> Cursor -> (# Status, Addr#, Cursor #)
eatSpaces pos end cur =
  if notLtAddr pos end then
    (# Good, pos, cur #)

  else
    case indexWord8OffAddr# pos 0# of
      0x20#Word8 {-   -}  -> eatSpaces (plusAddr# pos 1#) end (slide cur 1#Word64)
      0x0A#Word8 {- \n -} -> eatSpaces (plusAddr# pos 1#) end (newline cur)
      0x7B#Word8 {- { -}  -> eatMultiComment pos end cur
      0x2D#Word8 {- - -}  ->
        let !pos1 = plusAddr# pos 1# in
        if ltAddr pos1 end && eqIndex pos1 0# 0x2D#Word8 {- - -}
        then eatLineComment (plusAddr# pos 2#) end (slide cur 2#Word64)
        else (# Good, pos, cur #)

      0x0D#Word8 {- \r -} -> eatSpaces (plusAddr# pos 1#) end cur
      0x09#Word8 {- \t -} -> (# HasTab, pos, cur #)
      _                   -> (# Good, pos, cur #)



-- LINE COMMENTS


eatLineComment :: Addr# -> Addr# -> Cursor -> (# Status, Addr#, Cursor #)
eatLineComment pos end cur =
  if notLtAddr pos end then
    (# Good, pos, cur #)

  else
    case indexWord8OffAddr# pos 0# of
      0x0A#Word8 {-\n-} ->
        eatSpaces (plusAddr# pos 1#) end (newline cur)

      word ->
        let !newPos = P.skipUtf8 pos end word in
        if ltAddr pos newPos
        then eatLineComment newPos end (slide cur 1#Word64)
        else (# HasNonUtf8, pos, cur #)



-- MULTI COMMENTS


eatMultiComment :: Addr# -> Addr# -> Cursor -> (# Status, Addr#, Cursor #)
eatMultiComment pos end cur =
  let
    !pos2 = plusAddr# pos 2#
  in
  if notLtAddr pos2 end then
    (# Good, pos, cur #)

  else if eqIndex pos 1# 0x2D#Word8 {- - -} then

    if eqIndex pos2 0# 0x7C#Word8 {- | -} then
      (# Good, pos, cur #)
    else
      let
        !(# status, newPos, newCur #) =  eatMultiCommentHelp pos2 end (slide cur 2#Word64) 1
      in
      case status of
        MultiGood    -> eatSpaces newPos end newCur
        MultiTab     -> (# HasTab, newPos, newCur #)
        MultiNonUtf8 -> (# HasNonUtf8, newPos, newCur #)
        MultiEndless -> (# EndlessMultiComment, pos, cur #)

  else
    (# Good, pos, cur #)


type MultiStatus :: UnliftedType
data MultiStatus
  = MultiGood
  | MultiTab
  | MultiNonUtf8
  | MultiEndless


eatMultiCommentHelp :: Addr# -> Addr# -> Cursor -> Word -> (# MultiStatus, Addr#, Cursor #)
eatMultiCommentHelp pos end cur openComments =
  if notLtAddr pos end
  then (# MultiEndless, pos, cur #)
  else
    case indexWord8OffAddr# pos 0# of
      0x0A#Word8 {-\n-} ->
        eatMultiCommentHelp (plusAddr# pos 1#) end (newline cur) openComments

      0x09#Word8 {-\t-} ->
        (# MultiTab, pos, cur #)

      0x2D#Word8 {---} | isPos1 0x7D#Word8 {- } -} ->
        if openComments == 1
        then (# MultiGood, plusAddr# pos 2#, slide cur 2#Word64 #)
        else eatMultiCommentHelp (plusAddr# pos 2#) end (slide cur 2#Word64) (openComments - 1)

      0x7B#Word8 {- { -} | isPos1 0x2D#Word8 {---} ->
        eatMultiCommentHelp (plusAddr# pos 2#) end (slide cur 2#Word64) (openComments + 1)

      word ->
        let !newPos = P.skipUtf8 pos end word in
        if ltAddr pos newPos
        then eatMultiCommentHelp newPos end (slide cur 1#Word64) openComments
        else (# MultiNonUtf8, pos, cur #)
  where
    isPos1 w =
      let
        !pos1 = plusAddr# pos 1#
      in
      ltAddr pos1 end && eqIndex pos1 0# w




-- DOCUMENTATION COMMENT


docComment :: (Cursor -> x) -> (E.Space -> Cursor -> x) -> P.Parser x Src.Comment
docComment toExpectation toSpaceError =
  P.Parser $ \fpc (P.State pos end indent cur) cok _ cerr eerr ->
    let
      !pos3 = plusAddr# pos 3#
    in
    if leAddr pos3 end
      && eqIndex pos 0# 0x7B#Word8 {- { -}
      && eqIndex pos 1# 0x2D#Word8 {- - -}
      && eqIndex pos 2# 0x7C#Word8 {- | -}
    then
      let
        !cur3 = slide cur 3#Word64
        !(# status, newPos, newCur #) =  eatMultiCommentHelp pos3 end cur3 1
      in
      case status of
        MultiGood ->
          let
            !snippet = P.Snippet fpc pos3 (plusAddr# newPos (-2#)) cur3
            !comment = Src.Comment snippet
            !newState = P.State newPos end indent newCur
          in
          cok comment newState

        MultiTab     -> cerr newCur (toSpaceError E.HasTab)
        MultiNonUtf8 -> cerr newCur (toSpaceError E.HasNonUtf8)
        MultiEndless -> cerr cur    (toSpaceError E.EndlessMultiComment)
    else
      eerr cur toExpectation

