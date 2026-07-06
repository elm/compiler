{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, OverloadedStrings, UnboxedTuples #-}
module Parse.Variable
  ( lower
  , upper
  , moduleName
  , Upper(..)
  , foreignUpper
  , foreignAlpha
  --
  , reservedWords
  --
  , chompInnerChars
  )
  where


import qualified Data.Name as Name
import qualified Data.Set as Set
import GHC.Prim

import qualified AST.Source as Src
import qualified AST.Prim.Variable as Var
import Parse.Primitives (Parser, Cursor, eqAddr, ltAddr, eqIndex, slide)
import qualified Parse.Primitives as P



-- LOCAL UPPER


upper :: (Cursor -> x) -> Parser x Name.Name
upper toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !(# newPos, newCur #) = chompUpper pos end cur in
    if eqAddr pos newPos then
      eerr cur toError
    else
      do  name <- Name.fromAddr pos newPos
          cok name (P.State newPos end indent newCur)



-- LOCAL LOWER


lower :: (Cursor -> x) -> Parser x Name.Name
lower toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !(# newPos, newCur #) = chompLower pos end cur in
    if eqAddr pos newPos then
      eerr cur toError
    else
      do  name <- Name.fromAddr pos newPos
          if Set.member name reservedWords
            then eerr cur toError
            else
              let
                !newState = P.State newPos end indent newCur
              in
              cok name newState


{-# NOINLINE reservedWords #-}
reservedWords :: Set.Set Name.Name  -- PERF try using a trie instead
reservedWords =
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



-- MODULE NAME


moduleName :: (Cursor -> x) -> Parser x Name.Name
moduleName toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    let
      !(# pos1, cur1 #) = chompUpper pos end cur
    in
    if eqAddr pos pos1 then
      eerr cur toError
    else
      let
        !(# status, newPos, newCur #) = moduleNameHelp pos1 end cur1
      in
      case status of
        Good ->
          do  name <- Name.fromAddr pos newPos
              let !newState = P.State newPos end indent newCur
              cok name newState

        Bad ->
          cerr newCur toError


data ModuleNameStatus
  = Good
  | Bad


moduleNameHelp :: Addr# -> Addr# -> Cursor -> (# ModuleNameStatus, Addr#, Cursor #)
moduleNameHelp pos end cur =
  if isDot pos end then
    let
      !pos1 = plusAddr# pos 1#
      !(# newPos, newCur #) = chompUpper pos1 end (slide cur 1#Word64)
    in
    if eqAddr pos1 newPos then
      (# Bad, newPos, newCur #)
    else
      moduleNameHelp newPos end newCur

  else
    (# Good, pos, cur #)



-- FOREIGN UPPER


data Upper
  = Unqualified Name.Name
  | Qualified Name.Name Name.Name


foreignUpper :: (Cursor -> x) -> Parser x Upper
foreignUpper toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !(# upperStart, upperEnd, newCur #) = foreignUpperHelp pos end cur in
    if eqAddr upperStart upperEnd then
      eerr newCur toError
    else
      do  let !newState = P.State upperEnd end indent newCur
          !upperName <-
            if eqAddr upperStart pos
            then Unqualified <$> Name.fromAddr upperStart upperEnd
            else
              do  !home <- Name.fromAddr pos (plusAddr# upperStart (-1#))
                  !name <- Name.fromAddr upperStart upperEnd
                  return (Qualified home name)
          cok upperName newState


foreignUpperHelp :: Addr# -> Addr# -> Cursor -> (# Addr#, Addr#, Cursor #)
foreignUpperHelp pos end cur =
  let
    !(# newPos, newCur #) = chompUpper pos end cur
  in
  if eqAddr pos newPos then
    (# pos, pos, cur #)

  else if isDot newPos end then
    foreignUpperHelp (plusAddr# newPos 1#) end (slide newCur 1#Word64)

  else
    (# pos, newPos, newCur #)



-- FOREIGN ALPHA


foreignAlpha :: (Cursor -> x) -> Parser x Src.Expr_
foreignAlpha toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !(# alphaStart, alphaEnd, newCur, varType #) = foreignAlphaHelp pos end cur in
    if eqAddr alphaStart alphaEnd then
      eerr newCur toError
    else
      do  let !newState = P.State alphaEnd end indent newCur
          name <- Name.fromAddr alphaStart alphaEnd
          if eqAddr alphaStart pos
            then
              if Set.member name reservedWords
              then eerr cur toError
              else cok (Src.Var varType name) newState
            else
              do  home <- Name.fromAddr pos (plusAddr# alphaStart (-1#))
                  cok (Src.VarQual varType home name) newState


foreignAlphaHelp :: Addr# -> Addr# -> Cursor -> (# Addr#, Addr#, Cursor, Src.VarType #)
foreignAlphaHelp pos end cur =
  let
    !(# lowerPos, lowerCur #) = chompLower pos end cur
  in
  if ltAddr pos lowerPos then
    (# pos, lowerPos, lowerCur, Src.LowVar #)

  else
    let
      !(# upperPos, upperCur #) = chompUpper pos end cur
    in
    if eqAddr pos upperPos then
      (# pos, pos, cur, Src.CapVar #)

    else if isDot upperPos end then
      foreignAlphaHelp (plusAddr# upperPos 1#) end (slide upperCur 1#Word64)

    else
      (# pos, upperPos, upperCur, Src.CapVar #)



---- CHAR CHOMPERS ----



-- DOTS


{-# INLINE isDot #-}
isDot :: Addr# -> Addr# -> Bool
isDot pos end =
  ltAddr pos end && eqIndex pos 0# 0x2e#Word8 {-.-}



-- UPPER CHARS


chompUpper :: Addr# -> Addr# -> Cursor -> (# Addr#, Cursor #)
chompUpper pos end cur =
  if ltAddr pos end
  then
    let
      !next = Var.chompUpper pos end (indexWord8OffAddr# pos 0#)
    in
    if eqAddr pos next
    then (# pos, cur #)
    else chompInnerChars next end (slide cur 1#Word64)
  else
    (# pos, cur #)



-- LOWER CHARS


chompLower :: Addr# -> Addr# -> Cursor -> (# Addr#, Cursor #)
chompLower pos end cur =
  if ltAddr pos end
  then
    let
      !next = Var.chompLower pos end (indexWord8OffAddr# pos 0#)
    in
    if ltAddr pos next
    then chompInnerChars next end (slide cur 1#Word64)
    else (# pos, cur #)
  else
    (# pos, cur #)



-- INNER CHARS


chompInnerChars :: Addr# -> Addr# -> Cursor -> (# Addr#, Cursor #)
chompInnerChars pos end !cur =
  if ltAddr pos end
  then
    let
      !next = Var.chompInner pos end (indexWord8OffAddr# pos 0#)
    in
    if ltAddr pos next
    then chompInnerChars next end (slide cur 1#Word64)
    else (# pos, cur #)
  else
    (# pos, cur #)

