{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, OverloadedStrings,
QuasiQuotes, TemplateHaskell, UnboxedTuples
#-}
module Generate.JavaScript.Name
  ( Name
  , toBuilder
  , fromIndex
  , fromInt
  , fromLocal
  , fromGlobal
  , fromCycle
  , fromKernel
  , makeF
  , makeA
  , makeLabel
  , makeTemp
  , dollar
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.Int (Int(..))
import GHC.Prim
import GHC.ST (ST(ST), runST)
import GHC.Word (Word8(..))

import qualified Crash
import qualified String as S

import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Data.Index as Index
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg



-- NAME


newtype Name =
  Name { toBuilder :: B.Builder }



-- CONSTRUCTORS


fromIndex :: Index.ZeroBased -> Name
fromIndex index =
  fromInt (Index.toMachine index)


fromInt :: Int -> Name
fromInt n =
  Name (N.toBuilder (intToAscii n))


fromLocal :: N.Name -> Name
fromLocal name =
  if Set.member name reservedNames then
    Name ("_" <> N.toBuilder name)
  else
    Name (N.toBuilder name)


fromGlobal :: ModuleName.Canonical -> N.Name -> Name
fromGlobal home name =
  Name $ homeToBuilder home <> usd <> N.toBuilder name


fromCycle :: ModuleName.Canonical -> N.Name -> Name
fromCycle home name =
  Name $ homeToBuilder home <> "$cyclic$" <> N.toBuilder name


fromKernel :: Module.Kernel -> N.Name -> Name
fromKernel home name =
  Name ("_" <> Module.kernelToBuilder home <> "_" <> N.toBuilder name)


{-# INLINE homeToBuilder #-}
homeToBuilder :: ModuleName.Canonical -> B.Builder
homeToBuilder (ModuleName.Canonical pkg home) =
  usd <> Pkg.toJavaScriptBuilder pkg <>
  usd <> S.toEscapedBuilder 0x2E#Word8 {-.-} 0x24#Word8 {-$-} (Module.toString home)



-- TEMPORARY NAMES


makeF :: Int -> Name
makeF n =
  Name ("F" <> B.intDec n)


makeA :: Int -> Name
makeA n =
  Name ("A" <> B.intDec n)


makeLabel :: N.Name -> Int -> Name
makeLabel name index =
  Name (N.toBuilder name <> usd <> B.intDec index)


makeTemp :: N.Name -> Name
makeTemp name =
  Name ("$temp$" <> N.toBuilder name)


dollar :: Name
dollar =
  Name usd


usd :: B.Builder
usd =
  B.word8 0x24 {-$-}



-- RESERVED NAMES


{-# NOINLINE reservedNames #-}
reservedNames :: Set.Set N.Name
reservedNames =
  Set.union jsReservedWords elmReservedWords


jsReservedWords :: Set.Set N.Name
jsReservedWords =
  Set.fromList
    [ [N.ascii|do|], [N.ascii|if|], [N.ascii|in|]
    , [N.ascii|NaN|], [N.ascii|int|], [N.ascii|for|], [N.ascii|new|], [N.ascii|try|], [N.ascii|var|], [N.ascii|let|]
    , [N.ascii|null|], [N.ascii|true|], [N.ascii|eval|], [N.ascii|byte|], [N.ascii|char|], [N.ascii|goto|], [N.ascii|long|], [N.ascii|case|], [N.ascii|else|], [N.ascii|this|], [N.ascii|void|], [N.ascii|with|], [N.ascii|enum|]
    , [N.ascii|false|], [N.ascii|final|], [N.ascii|float|], [N.ascii|short|], [N.ascii|break|], [N.ascii|catch|], [N.ascii|throw|], [N.ascii|while|], [N.ascii|class|], [N.ascii|const|], [N.ascii|super|], [N.ascii|yield|]
    , [N.ascii|double|], [N.ascii|native|], [N.ascii|throws|], [N.ascii|delete|], [N.ascii|return|], [N.ascii|switch|], [N.ascii|typeof|], [N.ascii|export|], [N.ascii|import|], [N.ascii|public|], [N.ascii|static|]
    , [N.ascii|boolean|], [N.ascii|default|], [N.ascii|finally|], [N.ascii|extends|], [N.ascii|package|], [N.ascii|private|]
    , [N.ascii|Infinity|], [N.ascii|abstract|], [N.ascii|volatile|], [N.ascii|function|], [N.ascii|continue|], [N.ascii|debugger|], [N.ascii|function|]
    , [N.ascii|undefined|], [N.ascii|arguments|], [N.ascii|transient|], [N.ascii|interface|], [N.ascii|protected|]
    , [N.ascii|instanceof|], [N.ascii|implements|]
    , [N.ascii|synchronized|]
    ]


elmReservedWords :: Set.Set N.Name
elmReservedWords =
  Set.fromList
    [ [N.ascii|F2|], [N.ascii|F3|], [N.ascii|F4|], [N.ascii|F5|], [N.ascii|F6|], [N.ascii|F7|], [N.ascii|F8|], [N.ascii|F9|]
    , [N.ascii|A2|], [N.ascii|A3|], [N.ascii|A4|], [N.ascii|A5|], [N.ascii|A6|], [N.ascii|A7|], [N.ascii|A8|], [N.ascii|A9|]
    ]



-- INT TO ASCII


intToAscii :: Int -> N.Name
intToAscii n =
  if n < 53 then -- skip $ as a standalone name
    packName [toByte n]

  else
    intToAsciiHelp 2 (numStartBytes * numInnerBytes) allBadFields (n - 53)


intToAsciiHelp :: Int -> Int -> [BadFields] -> Int -> N.Name
intToAsciiHelp width blockSize badFields n =
  case badFields of
    [] ->
      if n < blockSize then
        unsafeIntToAscii width [] n
      else
        intToAsciiHelp (width + 1) (blockSize * numInnerBytes) [] (n - blockSize)

    BadFields renamings : biggerBadFields ->
      let availableSize = blockSize - Map.size renamings in
      if n < availableSize then
        let name = unsafeIntToAscii width [] n in
        Map.findWithDefault name name renamings
      else
        intToAsciiHelp (width + 1) (blockSize * numInnerBytes) biggerBadFields (n - availableSize)


packName :: [Word8] -> N.Name
packName words0 =
  runST $ ST $ \s0 ->
    case newByteArray# len          s0 of { (# s1, mba #) ->
    case loop mba 0# words0         s1 of {    s2         ->
    case unsafeFreezeByteArray# mba s2 of { (# s3, ba  #) ->
      (# s3, N.fromString (S.String ba) #)
    }}}
  where
    !(I# len) = List.length words0

    loop mba i list s0 =
      case list of
        []         -> s0
        W8# w : ws ->
          case writeWord8Array# mba i w s0 of
            s1 -> loop mba (i +# 1#) ws s1



-- UNSAFE INT TO ASCII


unsafeIntToAscii :: Int -> [Word8] -> Int -> N.Name
unsafeIntToAscii width bytes n =
  if width <= 1 then
    packName (toByte n : bytes)
  else
    let
      (quotient, remainder) =
        quotRem n numInnerBytes
    in
    unsafeIntToAscii (width - 1) (toByte remainder : bytes) quotient



-- ASCII BYTES


numStartBytes :: Int
numStartBytes =
  54


numInnerBytes :: Int
numInnerBytes =
  64


toByte :: Int -> Word8
toByte n
  | n < 26  = fromIntegral (97 + n     ) {- lower -}
  | n < 52  = fromIntegral (65 + n - 26) {- upper -}
  | n == 52 = 95 {- _ -}
  | n == 53 = 36 {- $ -}
  | n < 64  = fromIntegral (48 + n - 54) {- digit -}
  | True    = $(Crash.crash 'toByte) $ "cannot convert int " ++ show n ++ " to ASCII"



-- BAD FIELDS


newtype BadFields =
  BadFields { _renamings :: Renamings }


type Renamings =
  Map.Map N.Name N.Name


allBadFields :: [BadFields]
allBadFields =
  let
    add keyword dict =
      Map.alter (Just . addRenaming keyword) (N.size keyword) dict
  in
    Map.elems $ Set.foldr add Map.empty jsReservedWords


addRenaming :: N.Name -> Maybe BadFields -> BadFields
addRenaming keyword maybeBadFields =
  let
    width = N.size keyword
    maxName = numStartBytes * numInnerBytes ^ (width - 1) - 1
  in
  case maybeBadFields of
    Nothing ->
      BadFields $ Map.singleton keyword (unsafeIntToAscii width [] maxName)

    Just (BadFields renamings) ->
      BadFields $ Map.insert keyword (unsafeIntToAscii width [] (maxName - Map.size renamings)) renamings
