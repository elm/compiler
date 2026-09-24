{-# LANGUAGE BangPatterns, ExtendedLiterals, ForeignFunctionInterface, MagicHash,
OverloadedStrings, QuasiQuotes, UnboxedTuples, UnliftedDatatypes, UnliftedFFITypes
#-}
module Parse.Variable
  ( lower
  , upper
  , moduleName
  , Upper(..)
  , foreignUpper
  , foreignAlpha
  --
  , isKeyword
  , keywordSet
  --
  , isDot
  , chompUpperVar
  , chompLowerVar
  , chompInners
  )
  where


import qualified Data.Set as Set
import Foreign.C.Types (CInt(..))
import GHC.Base (UnliftedType)
import GHC.Exts (isTrue#)
import GHC.Prim
import GHC.ST (ST(ST), runST)

import qualified AST.Source as Src
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Variable as Var
import Parse.Primitives (Parser, Cursor, eqIndex, eqAddr, ltAddr, slide)
import qualified Parse.Primitives as P



-- LOCAL UPPER


upper :: (Addr# -> Addr# -> IO name) -> (Cursor -> x) -> Parser x name
upper fromAddr toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !(# newPos, newCur #) = chompUpperVar pos end cur in
    if eqAddr pos newPos then
      eerr cur toError
    else
      do  let !newState = P.State newPos end indent newCur
          !name <- fromAddr pos newPos
          cok name newState



-- LOCAL LOWER


lower :: (Addr# -> Addr# -> IO name) -> (Cursor -> x) -> Parser x name
lower fromAddr toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !(# newPos, newCur #) = chompLowerVar pos end cur in
    if eqAddr pos newPos then
      eerr cur toError
    else
      if isKeyword pos newPos
      then eerr cur toError
      else
        do  let !newState = P.State newPos end indent newCur
            !name <- fromAddr pos newPos
            cok name newState



-- MODULE NAME


moduleName :: (Cursor -> x) -> Parser x Module.Name
moduleName toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ cerr eerr ->
    let
      !(# pos1, cur1 #) = chompUpperVar pos end cur
    in
    if eqAddr pos pos1 then
      eerr cur toError
    else
      let
        !(# status, newPos, newCur #) = moduleNameHelp pos1 end cur1
      in
      case status of
        Good ->
          do  let !newState = P.State newPos end indent newCur
              !name <- Module.fromAddr pos newPos
              cok name newState

        Bad ->
          cerr newCur toError


type ModuleNameStatus :: UnliftedType
data ModuleNameStatus
  = Good
  | Bad


moduleNameHelp :: Addr# -> Addr# -> Cursor -> (# ModuleNameStatus, Addr#, Cursor #)
moduleNameHelp pos end cur =
  if isDot pos end then
    let
      !pos1 = plusAddr# pos 1#
      !(# newPos, newCur #) = chompUpperVar pos1 end (slide cur 1#Word64)
    in
    if eqAddr pos1 newPos then
      (# Bad, newPos, newCur #)
    else
      moduleNameHelp newPos end newCur

  else
    (# Good, pos, cur #)



-- FOREIGN UPPER


data Upper name
  = Unqualified name
  | Qualified Module.Prefix name


foreignUpper :: (Addr# -> Addr# -> IO name) -> (Cursor -> x) -> Parser x (Upper name)
foreignUpper fromAddr toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let !(# upperStart, upperEnd, newCur #) = foreignUpperHelp pos end cur in
    if eqAddr upperStart upperEnd
    then eerr newCur toError
    else
      do  let !newState = P.State upperEnd end indent newCur
          !upperName <-
            if eqAddr upperStart pos
            then Unqualified <$> fromAddr upperStart upperEnd
            else
              do  !home <- Module.prefixFromAddr pos (plusAddr# upperStart (-1#))
                  !name <- fromAddr upperStart upperEnd
                  return (Qualified home name)
          cok upperName newState


foreignUpperHelp :: Addr# -> Addr# -> Cursor -> (# Addr#, Addr#, Cursor #)
foreignUpperHelp pos end cur
  | eqAddr pos newPos = (# pos, pos, cur #)
  | isDot newPos end  = foreignUpperHelp (plusAddr# newPos 1#) end (slide newCur 1#Word64)
  | otherwise         = (# pos, newPos, newCur #)
  where
    !(# newPos, newCur #) =  chompUpperVar pos end cur



-- FOREIGN ALPHA


foreignAlpha :: (Cursor -> x) -> Parser x Src.Expr_
foreignAlpha toError =
  P.Parser $ \_ (P.State pos end indent cur) cok _ _ eerr ->
    let
      !(# lowerPos, lowerCur #) = chompLowerVar pos end cur
    in
    if ltAddr pos lowerPos
    then
      if isKeyword pos lowerPos
      then eerr cur toError
      else
        do  !name <- N.fromAddr pos lowerPos
            cok (Src.Var Src.LowVar name) (P.State lowerPos end indent lowerCur)
    else
      let
        !(# upperPos, upperCur #) = chompUpperVar pos end cur
      in
      if eqAddr pos upperPos
      then eerr cur toError
      else loop pos pos upperPos end indent upperCur cok
  where
    loop moduleStart prevStart prevEnd end indent cur cok =
      if isDot prevEnd end
      then
        let
          !pos = plusAddr# prevEnd 1#
          !(# lowerPos, lowerCur #) = chompLowerVar pos end (slide cur 1#Word64)
        in
        if ltAddr pos lowerPos
        then
          do  !home <- Module.prefixFromAddr moduleStart prevEnd
              !name <- N.fromAddr pos lowerPos
              cok (Src.VarQual Src.LowVar home name) (P.State lowerPos end indent lowerCur)
        else
          let
            !(# upperPos, upperCur #) = chompUpperVar pos end (slide cur 1#Word64)
          in
          if ltAddr pos upperPos
          then loop moduleStart pos upperPos end indent upperCur cok
          else done moduleStart prevStart prevEnd end indent cur cok
      else
        done moduleStart prevStart prevEnd end indent cur cok

    done moduleStart upperStart upperEnd end indent cur cok =
      if eqAddr moduleStart upperStart
      then
        do  !name <- N.fromAddr upperStart upperEnd
            cok (Src.Var Src.CapVar name) (P.State upperEnd end indent cur)
      else
        do  !home <- Module.prefixFromAddr moduleStart (plusAddr# upperStart (-1#))
            !name <- N.fromAddr upperStart upperEnd
            cok (Src.VarQual Src.CapVar home name) (P.State upperEnd end indent cur)



--------------------------------------------------------------------------------
-- HELPERS ---------------------------------------------------------------------
--------------------------------------------------------------------------------



-- DOTS


{-# INLINE isDot #-}
isDot :: Addr# -> Addr# -> Bool
isDot pos end =
  ltAddr pos end && eqIndex pos 0# 0x2e#Word8 {-.-}



-- UPPER VAR


chompUpperVar :: Addr# -> Addr# -> Cursor -> (# Addr#, Cursor #)
chompUpperVar pos end cur =
  if ltAddr pos end
  then
    let
      !next = Var.chompUpper pos end (indexWord8OffAddr# pos 0#)
    in
    if eqAddr pos next
    then (# pos, cur #)
    else chompInners next end (slide cur 1#Word64)
  else
    (# pos, cur #)



-- LOWER VAR


chompLowerVar :: Addr# -> Addr# -> Cursor -> (# Addr#, Cursor #)
chompLowerVar pos end cur =
  if ltAddr pos end
  then
    let
      !next = Var.chompLower pos end (indexWord8OffAddr# pos 0#)
    in
    if ltAddr pos next
    then chompInners next end (slide cur 1#Word64)
    else (# pos, cur #)
  else
    (# pos, cur #)



-- INNERS


chompInners :: Addr# -> Addr# -> Cursor -> (# Addr#, Cursor #)
chompInners pos end !cur =
  if ltAddr pos end
  then
    let
      !next = Var.chompInner pos end (indexWord8OffAddr# pos 0#)
    in
    if ltAddr pos next
    then chompInners next end (slide cur 1#Word64)
    else (# pos, cur #)
  else
    (# pos, cur #)




--------------------------------------------------------------------------------
-- KEYWORDS --------------------------------------------------------------------
--------------------------------------------------------------------------------



-- KEYWORDS
--
-- Keywords are stored in "fast" order (first by length, then by memcmp) to get
-- fastest possible lookups available without a trie.
--
-- PERF try using a trie instead?


data Keywords =
  Keywords (SmallArray# Keyword)


type Keyword :: UnliftedType
data Keyword =
  Keyword Addr# Int#


{-# NOINLINE keywords #-}
keywords :: Keywords
keywords =
  runST $ ST $ \s ->
    case newSmallArray#       14# (Keyword "as"#       2#)  s  of { (# s1, sma #) ->
    case writeSmallArray# sma  1# (Keyword "if"#       2#)  s1 of {    s2         ->
    case writeSmallArray# sma  2# (Keyword "in"#       2#)  s2 of {    s3         ->
    case writeSmallArray# sma  3# (Keyword "of"#       2#)  s3 of {    s4         ->
    case writeSmallArray# sma  4# (Keyword "let"#      3#)  s4 of {    s5         ->
    case writeSmallArray# sma  5# (Keyword "case"#     4#)  s5 of {    s6         ->
    case writeSmallArray# sma  6# (Keyword "else"#     4#)  s6 of {    s7         ->
    case writeSmallArray# sma  7# (Keyword "port"#     4#)  s7 of {    s8         ->
    case writeSmallArray# sma  8# (Keyword "then"#     4#)  s8 of {    s9         ->
    case writeSmallArray# sma  9# (Keyword "type"#     4#)  s9 of {    s10        ->
    case writeSmallArray# sma 10# (Keyword "where"#    5#) s10 of {    s11        ->
    case writeSmallArray# sma 11# (Keyword "import"#   6#) s11 of {    s12        ->
    case writeSmallArray# sma 12# (Keyword "module"#   6#) s12 of {    s13        ->
    case writeSmallArray# sma 13# (Keyword "exposing"# 8#) s13 of {    s14        ->
    case unsafeFreezeSmallArray# sma s14                       of { (# s15, sa #) ->
      (# s15, Keywords sa #)
    }}}}}}}}}}}}}}}


keywordSet :: Set.Set [Char]
keywordSet =
  Set.fromList
    ["as","if","in","of"
    ,"let","case","else","port","then","type"
    ,"where","import","module","exposing"
    ]


isKeyword :: Addr# -> Addr# -> Bool
isKeyword pos end =
    search 0# (sizeofSmallArray# keywords# -# 1#)
  where
    !(Keywords keywords#) = keywords
    !vlen = minusAddr# end pos

    search lo hi =
      if isTrue# (hi <# lo)
      then False
      else
        let
          !mid = quotInt# (lo +# hi) 2#
        in
        case indexSmallArray# keywords# mid of
          (# Keyword addr len #)
            | isTrue# (vlen <# len) -> search lo (mid -# 1#)
            | isTrue# (vlen ># len) -> search (mid +# 1#) hi
            | otherwise ->
                case memcmp pos addr len of
                  v
                    | v < 0     -> search lo (mid -# 1#)
                    | v > 0     -> search (mid +# 1#) hi
                    | otherwise -> True


foreign import ccall unsafe "string.h memcmp"
  memcmp :: Addr# -> Addr# -> Int# -> CInt
